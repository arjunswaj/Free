package com.asb.free

import cats.{Id, InjectK, ~>}
import cats.data.EitherK
import cats.free.Free
import cats.free.Free.inject
import com.asb.free.search.engine.GoogleTools.{GoogleInterpreter, GoogleUtil, GoogleUtils}
import com.asb.free.search.engine.MicrosoftTools.{MicrosoftInterpreter, MicrosoftUtil, MicrosoftUtils}
import org.apache.commons.csv.CSVRecord

object SearchEngineUtil {

  case class SEResult(bing: Either[Exception, String], google: Either[Exception, String])

  sealed trait SearchTools[A]

  case class DoSearch(csvRecord: CSVRecord) extends SearchTools[SEResult]

  class SearchEngineRecordProcessors[F[_]](implicit I: InjectK[SearchTools, F]) {

    def processRecord(csvRecord: CSVRecord): Free[F, SEResult] =
      inject[SearchTools, F](DoSearch(csvRecord))

  }

  object SearchEngineRecordProcessors {
    def apply[F[_]](implicit I: InjectK[SearchTools, F]): SearchEngineRecordProcessors[F] = new SearchEngineRecordProcessors[F]
  }

  type GoogleOrMicrosoft[A] = EitherK[GoogleUtil, MicrosoftUtil, A]
  val interpreter: GoogleOrMicrosoft ~> Id = GoogleInterpreter or MicrosoftInterpreter

  def program(csvRecord: CSVRecord)(implicit G: GoogleUtils[GoogleOrMicrosoft], M: MicrosoftUtils[GoogleOrMicrosoft]):
  Free[GoogleOrMicrosoft, SEResult] = {
    import G._
    import M._
    for {
      goog <- searchGoogle(csvRecord)
      micr <- searchMicrosoft(csvRecord)
    } yield SEResult(goog, micr)
  }

  object SearchEngineInterpreter extends (SearchTools ~> Id) {
    override def apply[A](fa: SearchTools[A]): Id[A] = fa match {
      case DoSearch(csvRecord) =>
        implicit val googleUtils = GoogleUtils[GoogleOrMicrosoft]
        implicit val microsoftUtils = MicrosoftUtils[GoogleOrMicrosoft]
        program(csvRecord).foldMap(interpreter)
    }
  }

}
