package com.asb.free.search.engine

import cats.free.Free
import cats.free.Free.inject
import cats.{Id, InjectK, ~>}
import org.apache.commons.csv.CSVRecord

object GoogleTools {

  sealed trait GoogleUtil[A]

  case class SearchGoogle(csvRecord: CSVRecord) extends GoogleUtil[Either[Exception, String]]

  class GoogleUtils[F[_]](implicit I: InjectK[GoogleUtil, F]) {

    def searchGoogle(csvRecord: CSVRecord): Free[F, Either[Exception, String]] =
      inject(SearchGoogle(csvRecord))

  }

  object GoogleUtils {
    def apply[F[_]](implicit I: InjectK[GoogleUtil, F]): GoogleUtils[F] = new GoogleUtils[F]
  }

  object GoogleInterpreter extends (GoogleUtil ~> Id) {
    override def apply[A](fa: GoogleUtil[A]): Id[A] = fa match {
      case SearchGoogle(csvRecord) => Right(csvRecord.get(0))
    }
  }

}
