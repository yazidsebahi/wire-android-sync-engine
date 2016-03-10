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
package com.waz

import android.content.{ContentProvider, ContentValues, Context}
import android.database.{Cursor, MatrixCursor}
import android.net.Uri
import android.os.Parcel
import android.provider.ContactsContract
import android.provider.ContactsContract.DisplayNameSources
import com.waz.api.impl._
import com.waz.api.{CoreList, IConversation}
import com.waz.content.MsgCursor
import com.waz.model.{Contact, NameSource}
import com.waz.service.messages.MessageAndLikes
import com.waz.service.{ContactsColumns, ContactsService}
import com.waz.threading.Threading
import com.waz.utils.events.{EventContext, Subscription, FlatMapSignal, Signal}
import com.waz.utils.{Cleanup, Managed, returning}
import org.robolectric.shadows.ShadowContentResolver
import org.scalatest.enablers.{Emptiness, Length}

import scala.collection.breakOut
import scala.concurrent.duration._
import scala.language.implicitConversions

package object testutils {

  private implicit lazy val ec = Threading.Background

  object Implicits {
    implicit def iconv_to_conv(conv: IConversation): Conversation = conv.asInstanceOf[Conversation]
    implicit def apimsg_to_msg(msg: com.waz.api.Message): Message = msg.asInstanceOf[Message]
    implicit def apiuser_to_user(user: com.waz.api.User): User = user.asInstanceOf[User]
    implicit def apiml_to_ml(list: com.waz.api.MessagesList): MessagesList = list.asInstanceOf[MessagesList]
    implicit def apiim_to_im(im: com.waz.api.ImageAsset): ImageAsset = im.asInstanceOf[ImageAsset]

    implicit lazy val CursorEmptiness: Emptiness[Cursor] = new Emptiness[Cursor] {
      override def isEmpty(thing: Cursor): Boolean = thing.getCount == 0
    }

    implicit lazy val UiSignalEmptiness: Emptiness[api.UiSignal[_]] = new Emptiness[api.UiSignal[_]] {
      override def isEmpty(thing: api.UiSignal[_]): Boolean = thing.isEmpty
    }

    implicit lazy val CoreListLength = new Length[CoreList[_]] {
      override def lengthOf(obj: CoreList[_]): Long = obj.size()
    }

    implicit lazy val CoreListEmptiness: Emptiness[CoreList[_]] = new Emptiness[CoreList[_]] {
      override def isEmpty(thing: CoreList[_]): Boolean = thing.size() == 0
    }

    implicit class MessagesCursorSeq(list: MsgCursor) extends Seq[MessageAndLikes] {
      override def length: Int = list.size
      override def apply(idx: Int): MessageAndLikes = list(idx)
      override def iterator: Iterator[MessageAndLikes] = new Iterator[MessageAndLikes]() {
        var idx = 0
        override def hasNext: Boolean = idx < list.size
        override def next(): MessageAndLikes = {
          idx += 1
          list(idx - 1)
        }
      }
    }

    implicit class EnrichedInt(val a: Int) extends AnyVal {
      def times(f: => Unit): Unit = (1 to a) foreach (_ => f)
    }

    implicit class CoreListAsScala[A](list: CoreList[A]) {
      def asScala: Vector[A] = (0 until list.size).map(list.get)(breakOut)
    }

    implicit class RichCoreListSeq[T](list: CoreList[T]) extends Seq[T] {
      override def length: Int = list.size
      override def apply(idx: Int): T = list.get(idx)
      override def iterator: Iterator[T] = new Iterator[T]() {
        var idx = 0
        override def hasNext: Boolean = idx < list.size
        override def next(): T = {
          idx += 1
          list.get(idx - 1)
        }
      }
    }

    implicit class UiSignalIsSignal[A](val s: api.UiSignal[A]) extends AnyVal {
      def signal : Signal[A] = new Signal[A] with api.Subscriber[A] {
        var sub = Option.empty[api.Subscription]

        override def next(value: A): Unit = set(Some(value), Some(Threading.Ui))

        override def onWire(): Unit = sub = Some(s.subscribe(this))

        override def onUnwire(): Unit = {
          sub.foreach(_.cancel())
          sub = None
        }
      }
    }

    implicit class UiObservableCanBeSignal[A <: api.UiObservable](val a: A) extends AnyVal {
      def signal[B](f: A => B): Signal[B] = new Signal[B] with api.UpdateListener {

        override def updated(): Unit = set(Some(f(a)), Some(Threading.Ui))

        override def onWire(): Unit = {
          a.addUpdateListener(this)
          updated()
        }

        override def onUnwire(): Unit = a.removeUpdateListener(this)
      }

      def flatSignal[B](f: A => Signal[B]): Signal[B] = new FlatMapSignal(signal(identity), f)
    }

    implicit class SignalToSink[A](val signal: Signal[A]) extends AnyVal {
      def sink: SignalSink[A] = returning(new SignalSink[A])(_.subscribe(signal)(EventContext.Global))
    }

    class SignalSink[A] {
      private var sub = Option.empty[Subscription]
      def subscribe(s: Signal[A])(implicit ctx: EventContext): Unit = sub = Some(s(v => value = Some(v)))
      def unsubscribe: Unit = sub.foreach { s =>
        s.destroy()
        sub = None
      }
      @volatile private[testutils] var value = Option.empty[A]
      def current: Option[A] = value
    }
  }

  def withUpdate[T](signal: Signal[_])(body: => T)(implicit timeout: FiniteDuration = 5.seconds): T = {
    import com.waz.utils.events.EventContext.Implicits.global
    @volatile var updateCount = 0
    signal { _ => updateCount += 1 }
    RobolectricUtils.awaitUi(updateCount > 0)
    updateCount = 0

    val result = body

    RobolectricUtils.awaitUi(updateCount > 0, "Signal was not updated")

    result
  }

  import ContactsService.Col

  def prepareAddressBookEntries(emailAddresses: Seq[(String, String)], phoneNumbers: Seq[(String, String)])(implicit context: Context): Unit = {
    ShadowContentResolver.reset()
    ShadowContentResolver.registerProvider(ContactsContract.AUTHORITY, new ShadowContentProvider {
      override def query(uri: Uri, projection: Array[String], selection: String, selectionArgs: Array[String], sortOrder: String): Cursor = uri match {
        case ContactsService.Emails => cursor(Vector(Col.EmailAddress), emailAddresses map { case (a, b) => Seq(a, b) })
        case ContactsService.Phones => cursor(Vector(Col.PhoneNumber), phoneNumbers map { case (a, b) => Seq(a, b) })
        case _ => null
      }
    })
    context.getContentResolver.notifyChange(ContactsContract.Contacts.CONTENT_URI, null)
  }

  def prepareContacts(contacts: Contact*)(implicit context: Context): Unit = {
    ShadowContentResolver.reset()
    val emailAddresses = contacts flatMap (c => c.emailAddresses map (e => Seq(c.id.str, e.str)))
    val phoneNumbers = contacts flatMap (c => c.phoneNumbers map (p => Seq(c.id.str, p.str)))

    ShadowContentResolver.registerProvider(ContactsContract.AUTHORITY, new ShadowContentProvider {
      override def query(uri: Uri, projection: Array[String], selection: String, selectionArgs: Array[String], sortOrder: String): Cursor = {
        def checking(cols: String*) = returning(cols.toVector)(_ => require(projection.tail.toVector == cols, s"projections should match; expected ${cols.toVector.mkString(", ")} but received ${projection.toVector.mkString(", ")}"))
        uri match {
          case ContactsService.Emails => cursor(checking(Col.EmailAddress), emailAddresses)
          case ContactsService.Phones => cursor(checking(Col.PhoneNumber), phoneNumbers)
          case ContactsService.Contacts =>
            cursor(checking(Col.Name, Col.NameSource, Col.SortKeyPrimary), contacts.sortBy(_.sortKey).map(c => Seq(c.id.str, c.name, src(c), c.sortKey)))
        }
      }
    })

    context.getContentResolver.notifyChange(ContactsContract.Contacts.CONTENT_URI, null)
  }

  private def src(c: Contact) = (c.nameSource match {
    case NameSource.StructuredName => DisplayNameSources.STRUCTURED_NAME
    case NameSource.Nickname => DisplayNameSources.NICKNAME
    case _ => DisplayNameSources.UNDEFINED
  }).toString

  def withParcel[A](f: Parcel => A): A = Managed(Parcel.obtain).acquire(f)
  implicit lazy val ParcelCleanup: Cleanup[Parcel] = new Cleanup[Parcel] { def apply(a: Parcel): Unit = a.recycle() }

  trait ShadowContentProvider extends ContentProvider {
    override def getType(uri: Uri): String = ""
    override def update(uri: Uri, contentValues: ContentValues, s: String, strings: Array[String]): Int = 0
    override def insert(uri: Uri, contentValues: ContentValues): Uri = null
    override def delete(uri: Uri, s: String, strings: Array[String]): Int = 0
    override def onCreate(): Boolean = true

    def cursor(columns: Vector[String], values: TraversableOnce[TraversableOnce[String]]): Cursor =
      utils.returning(new MatrixCursor((ContactsColumns.ContactLookup +: columns).toArray)) { c =>
        values foreach { row =>
          val rowArr = row.toArray[AnyRef]
          require(rowArr.length == columns.size + 1, "number of columns did not match")
          c.addRow(rowArr)
        }
      }
  }
}
