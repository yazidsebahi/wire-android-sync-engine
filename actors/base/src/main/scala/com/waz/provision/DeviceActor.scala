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
package com.waz.provision

import java.io.{ByteArrayInputStream, File, FileInputStream}
import java.util.concurrent.atomic.AtomicBoolean

import akka.actor.SupervisorStrategy._
import akka.actor._
import android.content.Context
import android.database.sqlite.SQLiteDatabase
import android.net.Uri
import com.waz.api.MessageContent.{Image, Text}
import com.waz.api.OtrClient.DeleteCallback
import com.waz.api.ZMessagingApi.RegistrationListener
import com.waz.api._
import com.waz.api.impl.{AccentColor, DoNothingAndProceed, ZMessagingApi}
import com.waz.cache.LocalData
import com.waz.content.{Database, GlobalStorage, Mime}
import com.waz.model.ConversationData.ConversationType
import com.waz.model.UserData.ConnectionStatus
import com.waz.model.VoiceChannelData.ChannelState
import com.waz.model.{MessageContent => _, _}
import com.waz.service.PreferenceService.Pref
import com.waz.service._
import com.waz.sync.client.AssetClient
import com.waz.sync.client.AssetClient.{OtrAssetMetadata, OtrAssetResponse}
import com.waz.testutils.CallJoinSpy
import com.waz.testutils.Implicits.{CoreListAsScala, _}
import com.waz.threading.{CancellableFuture, DispatchQueueStats, Threading}
import com.waz.ui.UiModule
import com.waz.utils._
import com.waz.utils.RichFuture.traverseSequential
import com.waz.znet.ClientWrapper
import com.waz.znet.ZNetClient._
import org.threeten.bp

import scala.concurrent.Future.successful
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}
import scala.util.Random

object DeviceActor {
  def props(deviceName: String,
            application: Context,
            backend: BackendConfig = BackendConfig.EdgeBackend,
            otrOnly: Boolean = false,
            wrapper: ClientWrapper) =
  Props(new DeviceActor(deviceName, application, backend, otrOnly, wrapper)).withDispatcher("ui-dispatcher")
}

class DeviceActor(val deviceName: String,
                  val application: Context,
                  backend: BackendConfig = BackendConfig.EdgeBackend,
                  otrOnly: Boolean = false,
                  wrapper: ClientWrapper) extends Actor with ActorLogging {

  import ActorMessage._

  override val supervisorStrategy =
    OneForOneStrategy(maxNrOfRetries = 1, withinTimeRange = 10.seconds) {
      case exc: Exception =>
        log.error(exc, s"device actor '$deviceName' died")
        Stop
    }

  lazy val globalModule = new GlobalModule(application, backend) {
    ZMessaging.currentGlobal = this
    override lazy val storage: Database = new GlobalStorage(application, Random.nextInt().toHexString)
    override lazy val clientWrapper: ClientWrapper = wrapper
    if (otrOnly) prefs.editUiPreferences(_.putBoolean("zms_send_only_otr", true))
  }

  lazy val delayNextAssetPosting = new AtomicBoolean(false)

  lazy val instance: InstanceService = new InstanceService(application, globalModule, new ZMessaging(_, _, _) {

    override def metadata: MetaDataService = new MetaDataService(context) {
      override val cryptoBoxDirName: String = "otr_" + Random.nextInt().toHexString
      override lazy val deviceModel: String = deviceName
      override lazy val localBluetoothName: String = deviceName
    }

    override lazy val assetClient = new AssetClient(znetClient) {
      import Threading.Implicits.Background

      override def postOtrAsset(convId: RConvId, metadata: OtrAssetMetadata, data: LocalData, ignoreMissing: Boolean): ErrorOrResponse[OtrAssetResponse] =
        CancellableFuture.delay(if (delayNextAssetPosting.compareAndSet(true, false)) 10.seconds else Duration.Zero) flatMap (_ => super.postOtrAsset(convId, metadata, data, ignoreMissing))
    }

    override lazy val assetMetaData = new com.waz.service.assets.MetaDataService(context, cache, assetsStorage, assets) {
      override def loadMetaData(mime: Mime, data: LocalData): CancellableFuture[Option[AssetMetaData]] = mime match {
        case Mime.Audio() => CancellableFuture successful Some(AssetMetaData.Audio(bp.Duration.ofSeconds(300), Seq.empty))
        case Mime.Video() => CancellableFuture successful Some(AssetMetaData.Video(Dim2(320, 480), bp.Duration.ofSeconds(300)))
        case _ => super.loadMetaData(mime, data)
      }

      override def loadMetaData(mime: Mime, uri: Uri): CancellableFuture[Option[AssetMetaData]] = mime match {
        case Mime.Audio() => CancellableFuture successful Some(AssetMetaData.Audio(bp.Duration.ofSeconds(300), Seq.empty))
        case Mime.Video() => CancellableFuture successful Some(AssetMetaData.Video(Dim2(320, 480), bp.Duration.ofSeconds(300)))
        case _ => super.loadMetaData(mime, uri)
      }
    }
  }) {
    override val currentUserPref: Pref[String] = global.prefs.preferenceStringSignal("current_user_" + Random.nextInt().toHexString)
  }
  lazy val ui = new UiModule(instance)
  lazy val api = {
    val api = new ZMessagingApi()(ui)
    api.onCreate(application)
    api.onResume()
    api
  }
  lazy val zmessaging = api.zmessaging.get
  lazy val convs = api.getConversations
  lazy val archived = convs.getArchivedConversations
  lazy val channels = api.getActiveVoiceChannels
  lazy val spy = new CallJoinSpy
  lazy val search = api.search()

  //Using a large value so that the test processes will always timeout first, and not the remotes
  implicit val defaultTimeout = 5.minutes
  implicit val execContext = context.dispatcher.prepare()

  implicit def zmsDb: SQLiteDatabase = api.zmessaging.get.storage.dbHelper.getWritableDatabase

  val maxConnectionAttempts = 10

  def searchConvsListById(remoteId: RConvId): IConversation => Boolean = { conv =>
    conv.data.remoteId.str == remoteId.str || conv.data.id.str == remoteId.str
  }

  def searchConvsListByName(name: String): IConversation => Boolean = _.name == name

  def findConvById(remoteId: RConvId): IConversation = convs.find(searchConvsListById(remoteId)).orElse(archived.find(searchConvsListById(remoteId))).get
  def convExistsById(remoteId: RConvId): Boolean = convs.exists(searchConvsListById(remoteId)) || archived.exists(searchConvsListById(remoteId))
  def convExistsByName(name: String): Boolean = convs.exists(searchConvsListByName(name)) || archived.exists(searchConvsListByName(name))
  def findConvByName(name: String): IConversation = convs.find(searchConvsListByName(name)).orElse(archived.find(searchConvsListByName(name))).get

  override def receive = preProcess(active)

  @throws[Exception](classOf[Exception])
  override def postStop(): Unit = {
    api.onPause()
    api.onDestroy()
    api.zmessaging foreach { zms =>
      zms.syncContent.syncStorage { storage =>
        storage.getJobs foreach { job => storage.remove(job.id) }
      }
    }
    super.postStop()
  }

  /**
   * Intercepts all messages to perform common logic like logging, and then passes
   * the message onto the specified handleFunction
    *
    * @param handleFunction the partial function to carry out the actual message processing after pre-processing
   * @return a Receive partial function
   */
  def preProcess(handleFunction: Receive): Receive = {
    case message@_ =>
      log.info(s"Received message: $message")
      handleFunction(message)
  }

  /////////////////////////////////////////////////////////////////////
  /// Messages that respond immediately or handle their own delays
  /////////////////////////////////////////////////////////////////////
  def active: Receive = activeFuture orElse {
    case Echo(msg, _) =>
      sender ! Echo(msg, deviceName)

    case RegisterPhone(phone, code, name, color) =>
      val senderRef = sender()
      api.register(CredentialsFactory.phoneCredentials(phone, code), name, AccentColor(color), new RegistrationListener {
        override def onRegistered(user: Self): Unit = senderRef ! Successful
        override def onRegistrationFailed(code: Int, message: String, label: String): Unit =
          senderRef ! Failed(s"unable to register: $code, $message, $label")
      })

    case Login(email, pass) =>
      val senderRef = sender()
      if (api.getSelf.getUser != null) {
        sender ! Failed(s"Process is already logged in as user: ${api.getSelf.getEmail}")
      }
      api.login(CredentialsFactory.emailCredentials(email, pass), new LoginListener {
        override def onSuccess(user: Self): Unit = {
          senderRef ! Successful
        }

        override def onFailed(code: Int, message: String, label: String): Unit = {
          log.info(s"Failed login: $code, $message, $label")
          senderRef ! Failed(s"Failed login: $code, $message, $label")
        }
      })

    case SendRequest(userId) =>
      val senderRef = sender()
      Option(userId) match {
        case Some(_) =>
          api.getUser(userId.str).connect("meep")
          waitUntil(convs) { _.asScala.exists(_.getType == ConversationType.WaitForConnection) } onSuccess {
            case _ => senderRef ! Successful
          }
        case None =>
          senderRef ! Failed("UserId cannot be null")
      }

    case ResetChannel =>
      Option(channels.getOngoingCall).foreach(_.leave())
      sender() ! Successful

    case message@_ =>
      log.error(s"unknown remote api command '$message'")
      sender ! Failed(s"unknown remote api command '$message'")
  }

  /////////////////////////////////////////////////////////////////////
  /// Messages that respond in the future
  /////////////////////////////////////////////////////////////////////
  def activeFuture = FutureReceive {

    case GetUser =>
      waitUntil(api.getSelf)(_.getUser != null) map { self => Successful(self.getUser.getId) }

    case GetConv(name) =>
      waitUntil(convs)(_ => convExistsByName(name)) map { _ =>
        Successful(findConvByName(name).data.remoteId.str)
      }

    case GetMessages(rConvId) =>
      for {
        conv <- getConv(rConvId)
        idx <- zmessaging.messagesStorage.msgsIndex(conv.id)
        cursor <- idx.loadCursor
      } yield {

        ConvMessages(Array.tabulate(cursor.size) { i =>
          val m = cursor(i)
          MessageInfo(m.message.id, m.message.msgType)
        })
      }

    case CreateGroupConversation(users@_*) =>
      zmessaging.convsUi.createGroupConversation(ConvId(), users) map { _ => Successful }

    case ClearConversation(remoteId) =>
      whenConversationExists(remoteId) { conv =>
        conv.clear()
        Successful
      }

    case SendText(remoteId, msg) =>
      whenConversationExists(remoteId) { conv =>
        conv.sendMessage(new Text(msg))
        Successful
      }

    case DeleteMessage(convId, msgId) =>
      withConv(convId) { conv =>
        zmessaging.convsUi.deleteMessage(conv.id, msgId)
      }

    case AcceptConnection(userId) =>
      Option(userId) match {
        case Some(_) =>
          waitUntil(api.getUser(userId.str))(_.getConnectionStatus == ConnectionStatus.PendingFromOther) map { user =>
            user.acceptConnection()
            Successful
          }
        case None =>
          Future(Failed("UserId cannot be null"))
      }

    case CancelConnection(userId) =>
      Option(userId) match {
        case Some(_) =>
          waitUntil(api.getUser(userId.str))(_.getConnectionStatus == ConnectionStatus.PendingFromUser) map { user =>
            user.cancelConnection()
            Successful
          }
        case None =>
          Future(Failed("UserId cannot be null"))
      }

    case SendImage(remoteId, path) =>
      whenConversationExists(remoteId) { conv =>
        conv.sendMessage(new Image(ui.images.getOrCreateImageAssetFrom(IoUtils.toByteArray(new FileInputStream(path)))))
        Successful
      }

    case SendImageData(remoteId, bytes) =>
      withConv(remoteId) { conv =>
        zmessaging.convsUi.sendMessage(conv.id, new Image(ui.images.getOrCreateImageAssetFrom(bytes)))
      }

    case SendAsset(remoteId, bytes, mime, name, delay) =>
      getConv(remoteId) flatMap  { conv =>
        delayNextAssetPosting.set(delay)
        zmessaging.convsUi.sendMessage(conv.id, new MessageContent.Asset(impl.AssetForUpload(AssetId(), Some(name), Mime(mime), Some(bytes.length.toLong)) {
          _ => new ByteArrayInputStream(bytes)
        }, DoNothingAndProceed)).map(_.fold2(Failed("no message sent"), m => Successful(m.id.str)))
      }

    case CancelAssetUpload(messageId) =>
      zmessaging.messagesStorage.getMessage(messageId).mapSome(_.assetId).flatMapSome { assetId =>
        zmessaging.assets.cancelUpload(assetId, messageId)
      }.map {
        case Some(()) => Successful
        case None     => Failed("upload not canceled: message not found or no asset ID present")
      }

    case SendFile(remoteId, path, mime) =>
      withConv(remoteId) { conv =>
        val file = new File(path)
        val asset = impl.AssetForUpload(AssetId(), Some(file.getName), Mime(mime), Some(file.length)) {
          _ => new FileInputStream(file)
        }
        zmessaging.convsUi.sendMessage(conv.id, new MessageContent.Asset(asset, DoNothingAndProceed))
      }

    case AddMembers(remoteId, users@_*) =>
      withConv(remoteId) { conv =>
        zmessaging.convsUi.addConversationMembers(conv.id, users)
      }

    case Knock(remoteId) =>
      withConv(remoteId) { conv =>
        zmessaging.convsUi.knock(conv.id)
      }

    case Typing(remoteId) =>
      whenConversationExists(remoteId) { conv =>
        conv.getInputStateIndicator.textChanged()
        Successful
      }

    case ClearTyping(remoteId) =>
      whenConversationExists(remoteId) { conv =>
        conv.getInputStateIndicator.textCleared()
        Successful
      }

    case ArchiveConv(remoteId) =>
      whenConversationExists(remoteId) { conv =>
        conv.setArchived(true)
        Successful
      }

    case UnarchiveConv(remoteId) =>
      whenConversationExists(remoteId) { conv =>
        conv.setArchived(false)
        Successful
      }

    case MuteConv(remoteId) =>
      whenConversationExists(remoteId) { conv =>
        conv.setMuted(true)
        Successful
      }

    case UnmuteConv(remoteId) =>
      whenConversationExists(remoteId) { conv =>
        conv.setMuted(false)
        Successful
      }

    case UpdateProfileImage(path) =>
      whenSelfLoaded { self =>
        self.setPicture(api.ui.images.getOrCreateImageAssetFrom(IoUtils.toByteArray(getClass.getResourceAsStream(path))))
        Successful
      }

    case ClearProfileImage =>
      whenSelfLoaded { self =>
        self.clearPicture()
        Successful
      }

    case UpdateProfileName(name) =>
      whenSelfLoaded { self =>
        self.setName(name)
        Successful
      }

    case UpdateProfileColor(color) =>
      whenSelfLoaded { self =>
        self.setAccent(color)
        Successful
      }

    case UpdateProfileEmail(email) =>
      whenSelfLoaded { self =>
        self.setEmail(email, new CredentialsUpdateListener {
          override def onUpdateFailed(code: Int, message: String, label: String): Unit = ()

          override def onUpdated(): Unit = ()
        })
        Successful
      }

    case AcceptCall =>
      whenCallIncoming { channel =>
        channel.join(spy.joinCallback)
        Successful
      }

    case StartCall(remoteId) =>
      whenConversationExists(remoteId) { conv =>
        conv.getVoiceChannel.join(spy.joinCallback)
        Successful
      }

    case Disconnect =>
      waitUntil(channels)(_.hasOngoingCall) map { channels =>
        channels.getOngoingCall.leave()
        Successful
      }

    case WaitCalling =>
      whenCallIncoming { _ => Successful }

    case WaitDisconnected =>
      waitUntil(channels)(channels => !channels.hasIncomingCall && !channels.hasOngoingCall) map { _ => Successful }

    case SetMessageLiking(remoteId, messageId, action) =>
      waitUntil(convs)(_ => convExistsById(remoteId)) flatMap { _ =>
        val messages = findConvById(remoteId).getMessages
        waitUntil(messages)(_.exists(_.data.id == messageId)) map { messages =>
          messages.find(_.data.id == messageId).foreach(msg => if (action == Liking.Action.Like) msg.like() else msg.unlike())
          Successful
        }
      }

    case SetDeviceLabel(label) =>
      waitUntil(api.getSelf.getOtrClient)(!_.isEmpty) map { client =>
        client.get.setLabel(label)
        Successful
      }

    case DeleteDevice(clientId, password) =>
      def find(cs: CoreList[OtrClient]) = cs.find(_.asInstanceOf[com.waz.api.impl.otr.OtrClient].clientId.str == clientId)
      waitUntil(api.getSelf.getOtherOtrClients)(find(_).isDefined) map find flatMap {
        case None => successful(Failed(s"Client not found: $clientId"))
        case Some(client) =>
          val p = Promise[ActorMessage]()
          client.delete(password, new DeleteCallback {
            override def onClientDeleted(client: OtrClient): Unit = p trySuccess Successful
            override def onDeleteFailed(error: String): Unit = p trySuccess Failed(s"DeleteDevice failed with error: $error")
          })
          p.future
      }

    case DeleteAllOtherDevices(password) =>
      api.zmessaging.fold[Future[ActorMessage]](successful(Failed("no zmessaging"))) { zms =>
        for {
          Some(user)    <- zms.users.getSelfUserId
          Some(current) <- zms.otrClientsService.getSelfClient.map(_.map(_.id))
          clients       <- zms.otrClientsService.getClients(user).map(_.map(_.id))
          others         = clients.filter(current != _)
          responses     <- traverseSequential(others)(zms.otrClientsService.deleteClient(_, password))
          failures       = responses.collect { case Left(err) => s"[unable to delete client: ${err.message}, ${err.code}, ${err.label}]" }
        } yield if (failures.isEmpty) Successful else Failed(failures mkString ", ")
      }

    case GetDeviceId() =>
      waitUntil(api.getSelf.getOtrClient)(!_.isEmpty) map { client =>
        Successful(client.get.getId);
      }

    case GetDeviceFingerPrint() =>
      waitUntil(api.getSelf.getOtrClient)(!_.isEmpty) flatMap { client =>
        waitUntil(client.get.getFingerprint)(!_.isEmpty) map { fingerPrint =>
          Successful(new String(fingerPrint.get.getRawBytes))
        }
      }

    case AwaitSyncCompleted =>
      api.zmessaging.fold[Future[ActorMessage]](successful(Failed("no zmessaging"))) { zms =>
        zms.syncContent.syncJobs.filter(_.isEmpty).head map { _ => Successful }
      }

    case ResetQueueStats =>
      successful({
              com.waz.threading.DispatchQueueStats.reset()
              Successful
            })

    case GetQueueStats =>
      println(s"dispatch queue stats")
      DispatchQueueStats.printStats(10)
      successful({
              QueueStats(DispatchQueueStats.report(10).toArray)
            })
  }

  def whenConversationsLoaded(task: ConversationsList => ActorMessage): Unit = {
    waitUntil(convs)(_.size > 0) map task
  }

  def whenCallIncoming(task: VoiceChannel => ActorMessage): Future[Any] = {
    waitUntil(channels)(_.hasIncomingCall) flatMap { chs =>
      waitUntil(chs.getIncomingCall)(channel => channel.getState == ChannelState.OtherCalling || channel.getState == ChannelState.OthersConnected)
    } map task
  }

  def whenSelfLoaded(task: Self => ActorMessage): Future[Any] = {
    waitUntil(api.getSelf)(_.getUser != null) map task
  }

  def whenConversationExists(remoteId: RConvId)(task: IConversation => ActorMessage): Future[Any] = {
    Option(remoteId) match {
      case Some(_) =>
        waitUntil(convs)(_ => convExistsById(remoteId)) map { _ =>
          task(findConvById(remoteId))
        }
      case None =>
        Future(Failed("Conversation remoteId cannot be null"))
    }
  }

  def withConv[A](id: RConvId)(f: ConversationData => Future[A]) =
    getConv(id) flatMap f map { _ => Successful }

  def getConv(id: RConvId) = zmessaging.convsContent.convByRemoteId(id) flatMap {
    case None =>
      log.warning(s"rconv id not found: $id")
      zmessaging.convsContent.convById(ConvId(id.str)) collect { case Some(conv) => conv }
    case Some(conv) =>
      successful(conv)
  }

  def FutureReceive(receive: PartialFunction[Any, Future[Any]]): Receive = {
    case message if receive.isDefinedAt(message) => respondInFuture(receive(message))
  }

  def respondInFuture[S](f: Future[S]) = {
    val senderRef = sender()
    f.onComplete {
      case util.Success(message) =>
        senderRef ! message
      case util.Failure(cause) =>
        log.error(cause, "future receive failed")
        senderRef ! Failed(s"future receive failed: ${cause.getMessage}")
    }
  }

  private var listeners = Set.empty[UpdateListener] // required to keep references to the listeners as waitUntil manages to get them garbage-collected

  def waitUntil[S <: UiObservable](observable: S)(check: S => Boolean): Future[S] = {
    Threading.assertUiThread()
    val promise = Promise[S]()

    def onUpdated: Unit = {
      Threading.assertUiThread()
      if (check(observable)) {
        promise.trySuccess(observable)
      }
    }

    val listener = new UpdateListener {
      override def updated(): Unit = onUpdated
    }
    listeners += listener
    observable.addUpdateListener(listener)

    onUpdated

    promise.future.andThen {
      case _ =>
        observable.removeUpdateListener(listener)
        listeners -= listener
    }
  }
}
