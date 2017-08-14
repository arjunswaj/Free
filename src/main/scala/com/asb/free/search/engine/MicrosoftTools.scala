package com.asb.free.search.engine

import cats.free.Free
import cats.free.Free.inject
import cats.{Id, InjectK, ~>}
import org.apache.commons.csv.CSVRecord

object MicrosoftTools {

  sealed trait MicrosoftUtil[A]

  case class SearchMicrosoft(csvRecord: CSVRecord) extends MicrosoftUtil[Either[Exception, String]]

  class MicrosoftUtils[F[_]](implicit I: InjectK[MicrosoftUtil, F]) {

    def searchMicrosoft(csvRecord: CSVRecord): Free[F, Either[Exception, String]] =
      inject(SearchMicrosoft(csvRecord))

  }

  object MicrosoftUtils {
    def apply[F[_]](implicit I: InjectK[MicrosoftUtil, F]): MicrosoftUtils[F] = new MicrosoftUtils[F]
  }

  object MicrosoftInterpreter extends (MicrosoftUtil ~> Id) {
    override def apply[A](fa: MicrosoftUtil[A]): Id[A] = fa match {
      case SearchMicrosoft(csvRecord) => Right(csvRecord.get(1))
    }
  }

}
