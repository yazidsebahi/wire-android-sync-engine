/*
 * Wire
 * Copyright (C) 2016 Wire Swiss GmbH
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package com.waz.service.otr

import java.io._
import java.security.{DigestInputStream, DigestOutputStream, MessageDigest, SecureRandom}
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}
import javax.crypto.{Cipher, CipherInputStream, CipherOutputStream, Mac}

import android.content.Context
import android.util.Base64
import com.waz.HockeyApp
import com.waz.ZLog._
import com.waz.api.Verification
import com.waz.cache.{CacheService, LocalData}
import com.waz.content.MembersStorage
import com.waz.model.GenericMessage._
import com.waz.model._
import com.waz.model.otr._
import com.waz.service._
import com.waz.service.conversation.ConversationsContentUpdater
import com.waz.sync.SyncServiceHandle
import com.waz.sync.client.OtrClient
import com.waz.sync.client.OtrClient.EncryptedContent
import com.waz.threading.Threading
import com.waz.utils.events.{AggregatingSignal, EventContext, Signal}
import com.waz.utils.{IoUtils, LoggedTry, _}
import com.wire.cryptobox.{CryptoBox, CryptoException, PreKey}
import org.json.JSONObject
import org.threeten.bp.Instant

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.concurrent.Future.sequence
import scala.concurrent.duration._
import scala.util.{Success, Try}
import scala.{PartialFunction => =/>}

class OtrService(context: Context, userId: ZUserId, val clients: OtrClientsService, val content: OtrContentService,
    cryptoBox: CryptoBoxService, members: MembersStorage, keyValue: KeyValueService, users: UserService,
    convs: ConversationsContentUpdater, sync: SyncServiceHandle, cache: CacheService, metadata: MetaDataService,
    lifecycle: ZmsLifecycle) {

  import EventContext.Implicits.global
  import Threading.Implicits.Background
  import OtrService._

  private[waz] val lastPreKeyId = keyValue.keyValuePref("otr_last_prekey_id", 0)

  private lazy val clientLabel = if (metadata.localBluetoothName.isEmpty) metadata.deviceModel else metadata.localBluetoothName

  lazy val sessions = returning(new CryptoSessionService(cryptoBox)) { sessions =>
    // request self clients sync to update prekeys on backend
    // we've just created a session from message, this means that some user had to obtain our prekey from backend (so we can upload it)
    // using signal and sync interval parameter to limit requests to one an hour
    Signal.wrap(sessions.onCreateFromMessage).throttle(15.seconds) { _ => clients.requestSyncIfNeeded(1.hour) }
  }

  lifecycle.uiActive {
    case false => // ignore
    case true =>
      // ensure cryptobox is available
      cryptoBox.cryptoBox.onSuccess {
        case Some(_) => // loaded, great
        case None =>
          error(s"CryptoBox could not be loaded, removing current device")
          clients.onCurrentClientRemoved()
      }
  }

  def eventTransformer(events: Vector[Event]): Future[Vector[Event]] =
    content.currentClientId flatMap {
      case Some(clientId) =>
        @tailrec def transform(es: Vector[Event], accu: Vector[Future[Vector[Event]]]): Vector[Future[Vector[Event]]] =
          if (es.isEmpty) accu
          else {
            val ph = isOtrEvent(es.head)
            val (batch, remaining) = es.span(isOtrEvent(_) == ph)
            val batched = if (ph) collectEvents(clientId, batch) else Future.successful(batch)
            transform(remaining, accu :+ batched)
          }
        sequence(transform(events.toVector, Vector.empty)).map(_.flatten) andThen checkForErrorsAndSendAutomaticResponseToRecoverCryptoSession
      case None =>
        error(s"Current client is not registered, will not process any otr events.")
        Future.successful(events.iterator.filterNot(isOtrEvent).to[Vector])
    }

  private lazy val isOtrEvent: Event => Boolean = PartialFunction.cond(_) { case _: OtrEvent => true }

  private lazy val checkForErrorsAndSendAutomaticResponseToRecoverCryptoSession: Try[Vector[Event]] =/> Unit = {
    case Success(events) if metadata.internalBuild =>
      val errors = events collect {
        case GenericMessageEvent(_, conv, _, _, _, GenericMessage(_, err: OtrError), _) => (conv, err)
      }
      if (errors.nonEmpty) sendSessionRecovery(errors.groupBy(_._1).mapValues(_.map(_._2)))
  }

  private def collectEvents(clientId: ClientId, events: Vector[Event]): Future[Vector[Event]] =
    Future.traverse(events) {
      case ev @ OtrMessageEvent(id, conv, time, from, sender, `clientId`, data, extData) =>
        decryptOtrEvent(ev) map {
          case GenericMessage(msgId, External(key, sha)) =>
            val extMsg = decodeExternal(key, sha, extData).getOrElse {
              error(s"External message could not be decoded ${External(key, sha)}, data: $extData")
              GenericMessage(msgId, OtrError("symmetric decryption failed", from, sender))
            }
            Some(GenericMessageEvent(id, conv, EventId.Zero, time, from, extMsg, otr = true).withLocalTime(ev.localTime))
          case GenericMessage(_, Duplicate) => None
          case GenericMessage(mId, SessionReset) if metadata.internalBuild => // display session reset notifications in internal build
             Some(GenericMessageEvent(id, conv, EventId.Zero, time, from, new GenericMessage(mId, Text("System msg: session reset", Map.empty)), otr = true))
          case GenericMessage(mId, SessionReset) => None // ignore session reset notifications
          case msg =>
            Some(GenericMessageEvent(id, conv, EventId.Zero, time, from, msg, otr = true).withLocalTime(ev.localTime))
        }

      case ev @ OtrAssetEvent(_, conv, time, from, sender, `clientId`, dataId, meta, data) =>
        decryptOtrEvent(ev) map {
          case msg @ GenericMessage(id, Image(tag, width, height, origWidth, origHeight, mime, size, None, _)) =>
            error(s"got otr asset event without otr key: $msg")
            val img = ImageData(tag, mime, width, height, origWidth, origHeight, size, Some(dataId), sent = true, data64 = data flatMap { arr => LoggedTry(Base64.encodeToString(arr, Base64.DEFAULT)).toOption })
            Some(AssetAddEvent(ev.id, conv, EventId.Zero, time, from, AssetId(id.str), img).withLocalTime(ev.localTime))
          case GenericMessage(id, Image(tag, width, height, origWidth, origHeight, mime, size, Some(key), sha)) =>
            val img = data.fold {
              ImageData(tag, mime, width, height, origWidth, origHeight, size, Some(dataId), sent = true, otrKey = Some(key), sha256 = sha)
            } { img =>
              val data64 = if (sha.forall(_.str == sha2(img))) LoggedTry(Base64.encodeToString(decryptSymmetric(key, img), Base64.DEFAULT)).toOption else None
              ImageData(tag, mime, width, height, origWidth, origHeight, size, Some(dataId), sent = true, otrKey = Some(key), sha256 = sha, data64 = data64)
            }
            Some(AssetAddEvent(ev.id, conv, EventId.Zero, time, from, AssetId(id.str), img).withLocalTime(ev.localTime))

          case GenericMessage(_, Duplicate) => None

          case msg =>
            Some(GenericMessageEvent(ev.id, conv, EventId.Zero, time, from, msg, otr = true).withLocalTime(ev.localTime))
        }
      case ev: OtrEvent if ev.recipient != clientId =>
        verbose(s"Skipping otr event not intended for us: $ev")
        Future successful None
      case ev =>
        error(s"Unhandled OtrEvent: $ev")
        Future successful None
    } map (_.flatten)

  private def decodeExternal(key: OtrKey, sha: Option[Sha256], extData: Option[Array[Byte]]) =
    for {
      data  <- extData if sha.forall(_.matches(data))
      plain <- LoggedTry(OtrService.decryptSymmetric(key, data)).toOption
      msg  <- LoggedTry(GenericMessage.decode(plain)).toOption
    } yield msg

  private[otr] def decryptOtrEvent(ev: OtrEvent, retry: Int = 0): Future[GenericMessage] =
    clients.getOrCreateClient(ev.from, ev.sender) flatMap { _ =>
      decryptMessage(ev.from, ev.sender, ev.ciphertext)
        .recoverWith {
          case e: CryptoException =>
            import CryptoException.Code._
            e.code match {
              case DUPLICATE_MESSAGE | OUTDATED_MESSAGE =>
                verbose(s"detected duplicate message for event: $ev")
                Future successful GenericMessage(Uid(), Duplicate)
              case REMOTE_IDENTITY_CHANGED if retry < 3 =>
                reportOtrError(e, ev)
                warn(s"Remote identity changed, will drop a session and try decrypting again", e)
                for {
                  _ <- sessions.deleteSession(sessionId(ev.from, ev.sender))
                  _ <- clients.updateVerified(ev.from, ev.sender, verified = false)
                  res <- decryptOtrEvent(ev, retry + 1)
                } yield res
              case _ =>
                reportOtrError(e, ev)
                Future successful GenericMessage(Uid(), OtrError(e.getMessage, ev.from, ev.sender))
            }
      }
    }

  // update client info and send error report to hockey, we want client info to somehow track originating platform
  private def reportOtrError(e: CryptoException, ev: OtrEvent) = sync.syncClients(ev.from) map { _ =>
    clients.getClient(ev.from, ev.sender) foreach { client =>
      HockeyApp.saveException(e, s"event: $ev, client: $client")
    }
  }

  def generatePreKeysIfNeeded(remainingKeys: Seq[Int]): Future[Seq[PreKey]] = {

    val remaining = remainingKeys.toSeq.filter(_ <= CryptoBox.MAX_PREKEY_ID)

    val maxId = if (remaining.isEmpty) None else Some(remaining.max)

    // old version was not updating lastPreKeyId properly, we need to detect that and reset it to lastId on backend
    def shouldResetLastIdPref(lastId: Int) = maxId.exists(max => max > lastId && max < LocalPreKeysLimit / 2)

    if (remaining.size > LowPreKeysThreshold) Future.successful(Nil)
    else lastPreKeyId() flatMap { lastId =>
      val startId =
        if (lastId > LocalPreKeysLimit) 0
        else if (shouldResetLastIdPref(lastId)) maxId.fold(0)(_ + 1)
        else lastId + 1

      val count = PreKeysCount - remaining.size

      cryptoBox { cb =>
        val keys = cb.newPreKeys(startId, count).toSeq
        (lastPreKeyId := keys.last.id) map (_ => keys)
      } map { _ getOrElse Nil }
    }
  }

  def resetSession(conv: ConvId, user: UserId, client: ClientId) =
    for {
      _ <- sessions.deleteSession(sessionId(user, client))
      _ <- clients.updateVerified(user, client, verified = false)
      _ <- sync.syncPreKeys(user, Set(client))
      syncId <- sync.postSessionReset(conv, user, client)
    } yield syncId

  def hasSession(user: UserId, client: ClientId) = sessions.getSession(sessionId(user, client)).map(_.isDefined)

  def createClient() = cryptoBox { cb =>
    val (lastKey, keys) = (cb.newLastPreKey(), cb.newPreKeys(0, PreKeysCount))
    (lastPreKeyId := keys.last.id) map { _ =>
      (Client(ClientId(), clientLabel, metadata.deviceModel, Some(Instant.now), signalingKey = Some(SignalingKey()), verified = Verification.VERIFIED, devType = metadata.deviceClass), lastKey, keys.toSeq)
    }
  }

  def decryptGcm(data: Array[Byte], mac: Array[Byte]) = clients.getSelfClient map {
    case Some(client @ Client(_, _, _, _, _, _, Some(key), _, _)) =>
      verbose(s"decrypting gcm for client $client")
      if (hmacSha256(key, data).toSeq != mac.toSeq) {
        warn(s"gcm MAC doesn't match")
        None
      } else
        LoggedTry(new JSONObject(new String(decryptSymmetric(key.encKeyBytes, data), "utf8"))).toOption
    case c =>
      warn(s"can not decrypt gcm, no signaling key found: $c")
      None
  }

  def decryptMessage(user: UserId, clientId: ClientId, msg: Array[Byte]): Future[GenericMessage] =
    sessions.decryptMessage(sessionId(user, clientId), msg) .map { plain =>
      verbose(s"decrypted data len: ${plain.length}")
      GenericMessage(plain)
    }

  // Try sending a message to clients from which we just got a decryption error.
  // This is very ugly solution and will lead to infinite msg sending loops if msgs continue to fail o both ends
  // It should only be used in internal build
  private def sendSessionRecovery(errors: Map[RConvId, Seq[OtrError]]) = Future.traverse(errors) {
    case (convId, errs) =>
      convs.processConvWithRemoteId(convId, retryAsync = false) { conv =>
        Future.traverse(errs.groupBy(_.from)) { case (user, es) =>
          Future.traverse(es) { err =>
            sync.postSessionReset(conv.id, user, err.sender)
          }
        }
      }
  }

  def encryptTargetedMessage(user: UserId, client: ClientId, msg: GenericMessage): Future[Option[OtrClient.EncryptedContent]] = {
    val msgData = msg.toByteArray

    sessions.withSession(sessionId(user, client)) { session =>
      EncryptedContent(Map(user -> Map(client -> session.encrypt(msgData))))
    }
  }

  /**
    * @param useFakeOnError - when true, we will return bomb emoji as msg content on encryption errors (for failing client)
    * @param partialResult - partial content encrypted in previous run, we will use that instead of encrypting again when available
    */
  def encryptMessage(convId: ConvId, msg: GenericMessage, useFakeOnError: Boolean = false, partialResult: EncryptedContent = EncryptedContent.Empty): Future[OtrClient.EncryptedContent] = {
    val msgData = msg.toByteArray

    def previous(user: UserId, client: ClientId) =
      partialResult.content.get(user).flatMap(_.get(client)).filter(arr => arr.nonEmpty && arr != EncryptionFailedMsg)

    def encrypt(user: UserId, clients: Seq[Client]) = Future.traverse(clients) { client =>
      previous(user, client.id) match {
        case Some(bytes) => Future successful Some(client.id -> bytes)
        case None =>
          verbose(s"encrypt for client: $client")
          sessions.withSession(sessionId(user, client.id)) { session =>
            client.id -> session.encrypt(msgData)
          } recover {
            case e: Throwable =>
              HockeyApp.saveException(e, s"encryption failed for user: $user, client: $client")
              if (useFakeOnError) Some(client.id -> EncryptionFailedMsg)
              else None
          }
      }
    } map { ms => user -> ms.flatten.toMap }

    content.currentClientId flatMap {
      case Some(selfClientId) =>
        members.getActiveUsers(convId) flatMap { users =>
          verbose(s"active users: $users")
          Future.traverse(users) { user =>
            clients.getClients(user) flatMap { cs =>
              verbose(s"user clients: $cs")
              encrypt(user, cs.filter(_ != selfClientId))
            }
          } map (res => EncryptedContent(res.toMap.filter(_._2.nonEmpty)))
        }
      case None =>
        throw new Exception("Client is not registered")
    }
  }

  def encryptAssetData(id: AssetId, tag: String, key: OtrKey, data: LocalData): Future[(Sha256, LocalData)] = {

    def encryptFile() = {
      val entry = cache.createForFile()
      val mac = encryptSymmetric(key, data.inputStream, entry.outputStream)
      (mac, entry)
    }

    def encryptBytes() = {
      val bos = new ByteArrayOutputStream()
      val mac = encryptSymmetric(key, data.inputStream, bos)
      (mac, cache.addData(Uid().str, bos.toByteArray))
    }

    Future {
      data.byteArray.fold(encryptFile()){ _ => encryptBytes() }
    }(Threading.Background)
  }

  def deleteClients(userMap: Map[UserId, Seq[ClientId]]) =
    Future.traverse(userMap) {
      case (user, cs) =>
        clients.removeClients(user, cs) flatMap { _ =>
          Future.traverse(cs) { c => sessions.deleteSession(sessionId(user, c)) }
        }
    }

  def fingerprintSignal(userId: UserId, clientId: ClientId): Signal[Option[Array[Byte]]] =
    Signal(content.currentClientIdSignal, users.selfUserId.signal).flatMap {
      case (None, _) => Signal.empty
      case (Some(`clientId`), `userId`) => Signal.future(cryptoBox { cb => Future successful cb.getLocalFingerprint })
      case _ =>
        val sid = sessionId(userId, clientId)
        def fingerprint = sessions.withSession(sid)(_.getRemoteFingerprint)
        val stream = sessions.onCreate.filter(_ == sid).mapAsync(_ => fingerprint)

        new AggregatingSignal[Option[Array[Byte]], Option[Array[Byte]]](stream, fingerprint, (prev, next) => next)
    }
}

object OtrService {
  private implicit val logTag: LogTag = logTagFor[OtrService]

  val random = new SecureRandom

  val PreKeysCount = 100
  val LowPreKeysThreshold = 50
  val LocalPreKeysLimit = 16 * 1024
  val EncryptionFailedMsg = "\uD83D\uDCA3".getBytes("utf8")

  def sessionId(user: UserId, client: ClientId) = s"${user}_$client"

  def symmetricCipher(key: Array[Byte], iv: Array[Byte], mode: Int) = {
    val cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    val ivSpec = new IvParameterSpec(iv)
    val secret = new SecretKeySpec(key, "AES")
    cipher.init(mode, secret, ivSpec)
    cipher
  }

  def decryptSymmetric(key: Array[Byte], input: Array[Byte]): Array[Byte] =
    symmetricCipher(key, input.take(16), Cipher.DECRYPT_MODE).doFinal(input.drop(16))

  def encryptSymmetric(key: OtrKey, bytes: Array[Byte]): (Sha256, Array[Byte]) = {
    val os = new ByteArrayOutputStream()
    val sha = encryptSymmetric(key, new ByteArrayInputStream(bytes), os)
    (sha, os.toByteArray)
  }

  def encryptSymmetric(key: OtrKey, is: InputStream, os: OutputStream): Sha256 = {
    val out = new DigestOutputStream(os, MessageDigest.getInstance("SHA-256"))
    val iv = returning(new Array[Byte](16))(random.nextBytes)
    out.write(iv)
    IoUtils.copy(is, new CipherOutputStream(out, symmetricCipher(key.bytes, iv, Cipher.ENCRYPT_MODE)))
    Sha256(out.getMessageDigest.digest())
  }

  def decryptSymmetric(key: OtrKey, input: Array[Byte]): Array[Byte] =
    symmetricCipher(key.bytes, input.take(16), Cipher.DECRYPT_MODE).doFinal(input.drop(16))

  def decryptSymmetric(key: OtrKey, is: InputStream, os: OutputStream): Sha256 = {
    val shaStream = new DigestInputStream(is, MessageDigest.getInstance("SHA-256"))
    val iv = returning(new Array[Byte](16))(shaStream.read(_)) // TODO: make sure 16 bytes were actually read
    IoUtils.copy(new CipherInputStream(shaStream, symmetricCipher(key.bytes, iv, Cipher.DECRYPT_MODE)), os)
    Sha256(shaStream.getMessageDigest.digest())
  }

  def hmacSha256(key: SignalingKey, data: Array[Byte]) = {
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(key.mac)
    mac.doFinal(data)
  }
}
