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
package com.waz.model

import com.waz.model.AssetMetaData.Image.Tag.{Medium, Preview}
import com.waz.model.AssetMetaData.Loudness
import com.waz.model.AssetStatus.{UploadInProgress, UploadNotStarted}
import com.waz.specs.AndroidFreeSpec
import com.waz.testutils.Matchers._
import com.waz.utils.sha2
import com.waz.utils.wrappers._
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, TableDrivenPropertyChecks}
import org.scalatest.{BeforeAndAfter, FeatureSpec, Matchers}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen.alphaNumChar
import org.scalacheck._
import org.scalamock.scalatest.MockFactory
import org.threeten.bp.Duration

class AssetDataSpec
  extends FeatureSpec with Matchers with TableDrivenPropertyChecks with BeforeAndAfter
    with GeneratorDrivenPropertyChecks with AndroidFreeSpec with MockFactory {

  import com.waz.model.AssetData._

  implicit def optGen[T](implicit gen: Gen[T]): Gen[Option[T]] = Gen.frequency((1, Gen.const(None)), (2, gen.map(Some(_))))

  implicit lazy val arbMetaData: Arbitrary[AssetMetaData] = Arbitrary(Gen.oneOf(arbImageMetaData.arbitrary, arbVideoMetaData.arbitrary, arbAudioMetaData.arbitrary))
  implicit lazy val arbImageMetaData: Arbitrary[AssetMetaData.Image] = Arbitrary(for (d <- arbitrary[Dim2]; t <- Gen.oneOf(Medium, Preview)) yield AssetMetaData.Image(d, t))
  implicit lazy val arbVideoMetaData: Arbitrary[AssetMetaData.Video] = Arbitrary(Gen.resultOf(AssetMetaData.Video(_: Dim2, _: Duration)))
  implicit lazy val arbAudioMetaData: Arbitrary[AssetMetaData.Audio] = Arbitrary(Gen.resultOf(AssetMetaData.Audio(_: Duration, _: Option[Loudness])))
  implicit lazy val arbDim2: Arbitrary[Dim2] = Arbitrary(for (w <- genDimension; h <- genDimension) yield Dim2(w, h))
  lazy val genDimension = Gen.chooseNum(0, 10000)
  implicit lazy val arbDuration: Arbitrary[Duration] = Arbitrary(Gen.posNum[Long] map Duration.ofMillis)
  implicit lazy val arbLoudness: Arbitrary[Loudness] = Arbitrary(for {
    len <- Gen.chooseNum(1,10)
    floats <- Gen.listOfN(len, Gen.chooseNum(0.0f, 1.0f))
  } yield Loudness(floats.toVector))

  implicit lazy val arbAssetData: Arbitrary[AssetData] = Arbitrary(for {
    id            <- arbitrary[AssetId]
    mime          <- Gen.oneOf(Seq(Mime.Default) ++ Mime.Video.supported ++ Mime.Image.supported) // no audio mime types
    sizeInBytes   <- Gen.posNum[Long]
    name          <- optGen(alphaNumStr)
    source        <- optGen(arbitrary[URI])
    proxyPath     <- optGen(arbitrary[String])
    convId        <- optGen(arbitrary[RConvId])
  } yield AssetData(id, mime, sizeInBytes, UploadNotStarted, None, None, None, None, None, name, None, None, source, None, convId, None))

  // TODO: Right now the default implicit AssetData JSON decoder is v2 AnyAssetDataDecoder. FInd a way to change it
  // so in tests we could use the following enlarged arbAssetData. Or wait for the end of the transition period.
  /*
  implicit lazy val arbAssetData: Arbitrary[AssetData] = Arbitrary(for {
    id            <- arbitrary[AssetId]
    mime          <- Gen.oneOf(knownMimeTypes)
    sizeInBytes   <- Gen.posNum[Long]
    status        <- arbitrary[AssetStatus]
    remoteId      <- optGen(arbitrary[RAssetId])
    token         <- optGen(arbitrary[AssetToken])
    otrKey        <- optGen(arbitrary[AESKey])
    sha           <- optGen(arbitrary[Sha256])
    encryption    <- optGen(Gen.oneOf(EncryptionAlgorithm.AES_GCM, EncryptionAlgorithm.AES_CBC))
    name          <- optGen(alphaNumStr)
    previewId     <- optGen(arbitrary[AssetId])
    metaData      <- optGen(arbitrary[AssetMetaData])
    source        <- optGen(arbitrary[URI])
    proxyPath     <- optGen(arbitrary[String])
    convId        <- optGen(arbitrary[RConvId])
    data <- optGen(arbitrary[Array[Byte]])
  } yield AssetData(id, mime, sizeInBytes, status, remoteId, token, otrKey, sha, encryption, name, previewId, metaData, source, proxyPath, convId, data))*/

  lazy val alphaNumStr = Gen.nonEmptyListOf(alphaNumChar).map(_.mkString)
  implicit lazy val arbAssetStatus: Arbitrary[AssetStatus] = Arbitrary(Gen.oneOf[AssetStatus](AssetStatus.UploadNotStarted,
    AssetStatus.UploadInProgress, AssetStatus.UploadCancelled, AssetStatus.UploadFailed, AssetStatus.UploadDone, AssetStatus.DownloadFailed))
  implicit lazy val arbAssetToken: Arbitrary[AssetToken] = Arbitrary(Gen.resultOf(AssetToken))
  implicit lazy val arbOtrKey: Arbitrary[AESKey] = Arbitrary(sideEffect(AESKey()))
  implicit lazy val arbSha256: Arbitrary[Sha256] = Arbitrary(arbitrary[Array[Byte]].map(b => Sha256(sha2(b))))
  implicit lazy val arbUri: Arbitrary[URI] = Arbitrary(for {
    scheme <- Gen.oneOf("file", "content", "http")
    path <- alphaNumStr
  } yield URI.parse(s"$scheme://$path"))
  def sideEffect[A](f: => A): Gen[A] = Gen.resultOf[Unit, A](_ => f)
  implicit lazy val arbConvId: Arbitrary[ConvId]         = Arbitrary(sideEffect(ConvId()))
  implicit lazy val arbRConvId: Arbitrary[RConvId]       = Arbitrary(sideEffect(RConvId()))
  implicit lazy val arbUserId: Arbitrary[UserId]         = Arbitrary(sideEffect(UserId()))
  implicit lazy val arbRAssetDataId: Arbitrary[RAssetId] = Arbitrary(sideEffect(RAssetId()))
  implicit lazy val arbAssetId: Arbitrary[AssetId]       = Arbitrary(sideEffect(AssetId()))

  feature("json serialization") {
    scenario("Random metadata") {
      forAll((_: AssetMetaData) should beUnchangedByEncodingAndDecoding[AssetMetaData])
    }

    // TODO: for audio mime types we don't decode the source. Write a test which checks that.
    scenario("Random non-audio assets") {
      forAll((_: AssetData) should beUnchangedByEncodingAndDecoding[AssetData])
    }

  }

  feature("Database") {

    scenario("Store a list of assets and retrieve them again") {

      implicit val db = mock[DB]

      val assets = (1 to 10).flatMap(_ => arbAssetData.arbitrary.sample).toVector
      val values = assets.map(AssetDataDao.values)

      val stmt = mock[DBStatement]
      val cursor = mock[DBCursor]
      inSequence {
        (db.delete _).expects("Assets", null, null)
        (db.inTransaction _).expects().returning(true)
        (db.compileStatement _).expects(*).returning(stmt)
        values.foreach(v => {
          (stmt.bindString _).expects(1, v.getAsString("_id"))
          (stmt.bindString _).expects(2, v.getAsString("asset_type"))
          (stmt.bindString _).expects(3, v.getAsString("data"))
          (stmt.execute _).expects()
        })
        (stmt.close _).expects()

        (db.query _).expects("Assets", null, null, null, null, null, null, null).returning(cursor)
        (cursor.moveToFirst _).expects()
        values.foreach(v => {
          (cursor.isClosed _).expects().returning(false)
          (cursor.isAfterLast _).expects().returning(false)
          (cursor.getColumnIndex _).expects("asset_type").returning(1)
          (cursor.getString _).expects(1).returning(v.getAsString("asset_type"))
          (cursor.getColumnIndex _).expects("data").returning(0)
          (cursor.getString _).expects(0).returning(v.getAsString("data"))
          (cursor.moveToNext _).expects().returning(true)
        })
        (cursor.isClosed _).expects().returning(false)
        (cursor.isAfterLast _).expects().returning(true)
        (cursor.close _).expects()
      }

      AssetDataDao.deleteAll
      AssetDataDao.insertOrReplace(assets)
      AssetDataDao.list shouldEqual assets
    }
  }


  /*feature("ImageAssetData") {

    scenario("Sort image with broken meta-data") {
      fail()
      //      val data = Seq(AssetData(metaData = Some(AssetMetaData.Image(Dim2(280, 280), "smallProfile")), remoteId = Some(RAssetId())), AssetData(metaData = Some(AssetMetaData.Image(Dim2(960, 960), "medium")), remoteId = Some(RAssetId())))
      //      data.sorted shouldEqual data
    }
  }*/

  feature("AnyAssetData.updated") {

    val id = AssetId()
    val conv = RConvId()
    val mime = Mime("text/plain")
    lazy val asset = AssetData(id = id, mime = mime, sizeInBytes = 100, convId = Some(conv), status = UploadInProgress, name = Some("file.txt"))

    //TODO Dean - test merging asset data
  }
}
