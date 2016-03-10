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
package com.waz.service

import android.content.Context
import com.softwaremill.macwire._
import com.waz.PermissionsService
import com.waz.ZLog._
import com.waz.api.NotificationsHandler.NotificationsHandlerFactory
import com.waz.api._
import com.waz.api.impl.{LogLevel, _}
import com.waz.bitmap.BitmapDecoder
import com.waz.cache.CacheService
import com.waz.client.RegistrationClient
import com.waz.content._
import com.waz.model._
import com.waz.model.otr.ClientId
import com.waz.service.assets.DownloaderService
import com.waz.service.call._
import com.waz.service.conversation._
import com.waz.service.images.{ImageAssetGenerator, ImageAssetService, ImageLoader}
import com.waz.service.invitations.InvitationService
import com.waz.service.media._
import com.waz.service.messages._
import com.waz.service.otr.{CryptoBoxService, OtrClientsService, OtrContentService, OtrService}
import com.waz.service.tracking.TrackingService
import com.waz.sync._
import com.waz.sync.client._
import com.waz.sync.handler._
import com.waz.sync.otr.{OtrClientsSyncHandler, OtrSyncHandler}
import com.waz.threading.{SerialDispatchQueue, Threading}
import com.waz.ui.{MemoryImageCache, UiModule}
import com.waz.utils.Locales
import com.waz.utils.events.{EventContext, Publisher, Signal}
import com.waz.znet.AuthenticationManager._
import com.waz.znet._
import net.hockeyapp.android.Constants

class GlobalModule(val context: Context, val backend: BackendConfig) {
  lazy val storage: Database = new GlobalStorage(context)
  lazy val prefs = wire[PreferenceService]
  lazy val metadata: MetaDataService = wire[MetaDataService]
  lazy val cache: CacheService = new CacheService(context, storage)
  lazy val gcmGlobal = wire[GcmGlobalService]
  lazy val bitmapDecoder: BitmapDecoder = wire[BitmapDecoder]
  lazy val imageCache: MemoryImageCache = wire[MemoryImageCache]
  lazy val network = wire[NetworkModeService]
  lazy val phoneNumbers: PhoneNumberService = wire[PhoneNumberService]
  lazy val timeouts = wire[Timeouts]
  lazy val permissions: PermissionsService = wire[PermissionsService]
  lazy val handlerFactory = ZMessaging.notificationsHandlerFactory(context)

  object globalImageLoader extends ImageLoader(context, downloader, cache, imageCache, new ImageAssetClient(new ZNetClient(this, "", ""), cache), bitmapDecoder, permissions) { override def tag = "Global" }

  lazy val decoder = Response.CacheResponseBodyDecoder(cache)
  lazy val loginClient = wire[LoginClient]
  lazy val regClient: RegistrationClient = wire[RegistrationClient]
  lazy val downloader: DownloaderService = wire[DownloaderService]
  lazy val cacheCleanup = wire[CacheCleaningService]

  lazy val users: ZUsers = wire[ZUsers]

  lazy val clientWrapper: ClientWrapper = ClientWrapper
  lazy val client: AsyncClient = new AsyncClient(decoder, AsyncClient.userAgent(metadata.appVersion.toString, ZmsVersion.ZMS_VERSION), clientWrapper)
}


class ZMessaging(val instance: InstanceService, initUser: ZUser, initToken: Option[Token] = None, val dbPrefix: String = "") {
  private implicit val logTag: LogTag = logTagFor[ZMessaging]
  private implicit val dispatcher = new SerialDispatchQueue(name = "ZMessaging")
  private implicit val ev = EventContext.Global

  def global = instance.global
  def context: Context = global.context
  def client = global.client
  def prefs = global.prefs
  def metadata = global.metadata
  def cache: CacheService = global.cache
  def cacheCleanup: CacheCleaningService = global.cacheCleanup
  def gcmGlobal = global.gcmGlobal
  def imageCache = global.imageCache
  def network = global.network
  def phoneNumbers = global.phoneNumbers
  def timeouts = global.timeouts
  def loginClient = global.loginClient
  def regClient = global.regClient
  def downloader = global.downloader
  def zusers = global.users
  def bitmapDecoder = global.bitmapDecoder
  def userId = initUser.id
  def backend = global.backend
  def permissions = global.permissions
  def handlerFactory = global.handlerFactory

  def otrClientId: Signal[Option[ClientId]] = otrContent.currentClientIdSignal

  lazy val user: ZUserService = new ZUserService(initUser, zusers, keyValue, instance)

  // storages
  lazy val storage: ZStorage = new ZStorage(initUser.id, context, dbPrefix)
  lazy val usersStorage: UsersStorage = wire[UsersStorage]
  lazy val membersStorage   = wire[MembersStorage]
  lazy val kvStorage        = wire[KeyValueStorage]
  lazy val assetsStorage    = wire[AssetsStorage]
  lazy val voiceStorage     = wire[VoiceChannelStorage]
  lazy val membersContent   = wire[MembersContentUpdater]
  lazy val messagesStorage  = new MessagesStorage(context, storage, users.getSelfUserId.map(_.getOrElse(UserId.Zero)), convsStorage, usersStorage, likings, messageAndLikesNotifier)
  lazy val likingsStorage: LikingsStorage = wire[LikingsStorage]
  lazy val voiceContent     = wire[VoiceChannelContent]
  lazy val notifStorage     = wire[NotificationStorage]
  lazy val otrClientsStorage = wire[OtrClientsStorage]

  lazy val convsStorage: ConversationStorage = wire[ConversationStorage]
  lazy val convsContent: ConversationsContentUpdater = wire[ConversationsContentUpdater]
  lazy val messagesContent: MessagesContentUpdater = wire[MessagesContentUpdater]

  lazy val znetClient = new ZNetClient(user, initToken, global.client, backend, loginClient, keyValue.accessTokenPref)

  lazy val spotifyClientId      = metadata.spotifyClientId

  // clients
  lazy val youtubeClient      = wire[YouTubeClient]
  lazy val soundCloudClient   = wire[SoundCloudClient]
  lazy val spotifyClient      = wire[SpotifyClient]
  lazy val imageClient        = wire[ImageAssetClient]
  lazy val usersClient        = wire[UsersClient]
  lazy val convClient         = wire[ConversationsClient]
  lazy val eventsClient       = wire[EventsClient]
  lazy val voiceClient        = wire[VoiceChannelClient]
  lazy val abClient           = wire[AddressBookClient]
  lazy val gcmClient          = wire[GcmClient]
  lazy val typingClient       = wire[TypingClient]
  lazy val invitationClient   = wire[InvitationClient]
  lazy val giphyClient        = wire[GiphyClient]
  lazy val userSearchClient   = wire[UserSearchClient]
  lazy val connectionsClient  = wire[ConnectionsClient]
  lazy val blacklistClient    = wire[VersionBlacklistClient]
  lazy val credentialsClient  = wire[CredentialsUpdateClient]
  lazy val messagesClient     = wire[MessagesClient]
  lazy val otrClient          = wire[com.waz.sync.client.OtrClient]

  // services
  lazy val keyValue: KeyValueService  = wire[KeyValueService]
  lazy val imageLoader    = wire[ImageLoader]

  lazy val push: PushService = wire[PushService]
  lazy val gcm: GcmService = wire[GcmService]
  lazy val errors         = wire[ErrorsService]
  lazy val reporting      = wire[ReportingService]
  lazy val mediamanager   = wire[MediaManagerService]
  lazy val lifecycle      = wire[ZmsLifecycle]
  lazy val websocket: WebSocketClientService = wire[WebSocketClientService]
  lazy val usersearch     = wire[UserSearchService]
  lazy val assetGenerator = wire[ImageAssetGenerator]
  lazy val imageAssets    = wire[ImageAssetService]
  lazy val users: UserService = wire[UserService]
  lazy val conversations: ConversationsService = wire[ConversationsService]
  lazy val convsNotifier  = wire[ConversationsNotifier]
  lazy val convEvents: ConversationEventsService = wire[ConversationEventsService]
  lazy val convsUi        = wire[ConversationsUiService]
  lazy val convsStats     = wire[ConversationsListStateService]
  lazy val messages: MessagesService = wire[MessagesService]
  lazy val messageAndLikesNotifier: MessageAndLikesNotifier = wire[MessageAndLikesNotifierImpl]
  lazy val connection: ConnectionService = wire[ConnectionService]
  lazy val flowmanager: FlowManagerService = wire[FlowManagerService]
  lazy val voice: VoiceChannelService = wire[VoiceChannelService]
  lazy val contacts: ContactsService = wire[ContactsService]
  lazy val typing: TypingService = wire[TypingService]
  lazy val blacklist      = wire[VersionBlacklistService]
  lazy val invitations    = wire[InvitationService]
  lazy val richmedia      = wire[RichMediaService]
  lazy val audiolink      = wire[AudioLinkService]
  lazy val tracking       = wire[TrackingService]
  lazy val giphy          = wire[GiphyService]
  lazy val youtubeMedia   = wire[YouTubeMediaService]
  lazy val soundCloudMedia = wire[SoundCloudMediaService]
  lazy val spotifyMedia   = wire[SpotifyMediaService]
  lazy val googleMapsMedia = wire[GoogleMapsMediaService]
  lazy val otrContent: OtrContentService = wire[OtrContentService]
  lazy val otrClientsService: OtrClientsService = wire[OtrClientsService]
  lazy val cryptoBox      = wire[CryptoBoxService]
  lazy val otrService: OtrService = wire[OtrService]
  lazy val genericMsgs: GenericMessageService = wire[GenericMessageService]
  lazy val likings: LikingsService = wire[LikingsService]
  lazy val notifications: NotificationService = wire[NotificationService]
  lazy val callLog = wire[CallLogService]

  // sync
  lazy val sync: SyncServiceHandle  = wire[AndroidSyncServiceHandle]
  lazy val syncHandler: SyncHandler   = wire[ZMessagingSyncHandler]
  lazy val syncRequests: SyncRequestService = new SyncRequestService(context, userId, storage, network, user, syncHandler, reporting)
  def syncContent = syncRequests.content

  lazy val imageassetSync   = wire[ImageAssetSyncHandler]
  lazy val usersearchSync   = wire[UserSearchSyncHandler]
  lazy val usersSync        = wire[UsersSyncHandler]
  lazy val conversationSync = wire[ConversationsSyncHandler]
  lazy val connectionsSync  = wire[ConnectionsSyncHandler]
  lazy val voicechannelSync = wire[VoiceChannelSyncHandler]
  lazy val addressbookSync  = wire[AddressBookSyncHandler]
  lazy val gcmSync          = wire[GcmSyncHandler]
  lazy val typingSync       = wire[TypingSyncHandler]
  lazy val richmediaSync    = wire[RichMediaSyncHandler]
  lazy val invitationSync   = wire[InvitationSyncHandler]
  lazy val blacklistSync    = wire[VersionBlacklistSyncHandler]
  lazy val messagesSync     = wire[MessagesSyncHandler]
  lazy val otrSync          = wire[OtrSyncHandler]
  lazy val otrClientsSync   = wire[OtrClientsSyncHandler]
  lazy val likingsSync      = wire[LikingsSyncHandler]
  lazy val lastReadSync     = wire[LastReadSyncHandler]
  lazy val clearedSync      = wire[ClearedSyncHandler]

  lazy val eventPipeline = new EventPipeline(Vector(otrService.eventTransformer), eventScheduler.enqueue)

  lazy val eventScheduler = {
    import EventScheduler._

    new EventScheduler(
      Stage(Sequential)(
        Stage(Interleaved)(
          connection.connectionEventsStage,
          connection.contactJoinEventsStage,
          users.userUpdateEventsStage,
          users.userDeleteEventsStage,
          flowmanager.callEventsStage,
          voice.callStateEventsStage,
          voice.memberLeaveEventsStage,
          conversations.convStateEventProcessingStage,
          typing.typingEventStage,
          otrClientsService.otrClientsProcessingStage,
          gcm.eventProcessingStage,
          Stage(Parallel)(
            UnarchivingEventProcessingStage(users, convsStorage),
            convEvents.conversationEventsStage,
            Stage(Interleaved)(
              messages.messageEventProcessingStage,
              genericMsgs.eventProcessingStage
            )
          )
        ),
        notifications.notificationEventsStage
      )
    )
  }

  // force loading of services which should run on start
  {
    conversations
    users
    websocket // connect on start

    // services listening on lifecycle verified login events
    contacts
    syncRequests

    // services listening for storage updates
    richmedia
  }
}


class ZUserService(@volatile var user: ZUser, zusers: ZUsers, keyValueService: KeyValueService, instanceService: InstanceService) extends CredentialsHandler {
  private implicit val tag = logTagFor[ZUserService]

  val signal = Signal(user)

  val onLogout = new Publisher[Unit]
  val onVerifiedLogin = Signal(Option.empty[ZUserId])
  onLogout { _ => onVerifiedLogin ! None } (EventContext.Global)

  override def userId: ZUserId = user.id

  override def credentials = user.email.map(EmailCredentials(_, user.password)) orElse user.phone.map(PhoneCredentials(_, None)) getOrElse {
    error(s"ZUser $user without any credentials found")
    EmailCredentials(EmailAddress(""), None)
  }

  override def cookie = user.cookie

  override def updateCookie(cookie: Cookie) = {
    println(s"updateCookie: $cookie, for user: $user")
    val userWasVerified = user.emailVerified
    user = user.copy(cookie = cookie, emailVerified = user.emailVerified || cookie.isDefined)
    signal ! user
    zusers.updateCookie(user, cookie)
    if (!userWasVerified && cookie.isDefined) notifyVerified()
  }

  override def onInvalidCredentials(id: ZUserId): Unit = {
    warn(s"onInvalidCredentials($id) called, current user: ${user.id}, verified: ${user.emailVerified}")
    if (user.emailVerified) instanceService.logout(id)
  }

  def update(u: ZUser): Unit = {
    debug(s"update($u), current: $user")
    require(u.id == user.id)
    val wasVerified = u.emailVerified

    user = u.copy(password = u.password.orElse(user.password), email = u.email, phone = u.phone, emailVerified = u.emailVerified, phoneVerified = u.phoneVerified)
    signal ! user

    if (!wasVerified && user.emailVerified) notifyVerified()
  }

  def logout() = instanceService.logout(user.id)

  private def notifyVerified() = onVerifiedLogin ! Some(user.id)
}


trait UserDataCallbacks {
  def updateZUserEmail(user: ZUser, email: EmailAddress, verified: Boolean): Unit
  def updateZUserPhone(user: ZUser, phone: PhoneNumber, verified: Boolean): Unit
}

object ZMessaging { self =>
  private implicit val logTag: LogTag = logTagFor(ZMessaging)

  require(LogLevel.initialized)

  private[waz] var context: Context = _

  private var backend = BackendConfig.DevBackend

  def useBackend(conf: BackendConfig) = {
    assert(context == null, "ZMessaging.useBackend should only be called before any ZMessagingApi instance is created, do that only once, asap in Application.onCreate")
    backend = conf
  }
  def useEdgeBackend(): Unit = useBackend(BackendConfig.EdgeBackend)
  def useProdBackend(): Unit = useBackend(BackendConfig.ProdBackend)

  private lazy val global: GlobalModule = new GlobalModule(context, backend)
  private lazy val instance: InstanceService = new InstanceService(context, global, new ZMessaging(_, _, _))
  private lazy val ui: UiModule = new UiModule(instance)

  // mutable for testing FIXME: get rid of that
  private [waz] var currentUi: UiModule = _
  private [waz] var currentGlobal: GlobalModule = _
  private [waz] var currentInstance: InstanceService = _

  def onCreate(context: Context) = {
    Threading.assertUiThread()

    if (this.currentUi == null) {
      this.context = context.getApplicationContext
      Constants.loadFromContext(context)
      currentUi = ui
      currentGlobal = global
      currentInstance = instance
      Threading.Background { Locales.preloadTransliterator() } // "preload"... - this should be very fast, normally, but slows down to 10 to 20 seconds when multidexed...
    }
  }

  type Factory = (InstanceService, ZUser, Option[Token]) => ZMessaging


  def notificationsHandlerFactory(context: Context) = context.getApplicationContext match { // TODO: use some registration mechanism instead of expecting the app to implement this interface
    case app: NotificationsHandlerFactory => app
    case app =>
      error(s"Application: '$app' doesn't implement NotificationsHandlerFactory")
      new NotificationsHandlerFactory {
        override def getCallingEventsHandler: CallingEventsHandler = EmptyEventsHandler
        override def getNotificationsHandler: NotificationsHandler = EmptyNotificationsHandler
      }
  }

  object EmptyEventsHandler extends CallingEventsHandler {
    override def onCallingEvent(event: CallingEvent): Unit = ()
  }

  object EmptyNotificationsHandler extends NotificationsHandler {
    override def updateGcmNotification(notifications: GcmNotificationsList): Unit = ()
    override def updateOngoingCallNotification(ongoingCall: NotificationsHandler.ActiveChannel, incomingCall: NotificationsHandler.ActiveChannel, isUiActive: Boolean): Unit = ()
  }
}
