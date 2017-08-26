package com.asb.free

import cats.data.EitherK
import cats.free.Free
import cats.free.Free.inject
import cats.{Id, InjectK, ~>}
import com.asb.free.SearchEngineUtil.{SEResult, SearchEngineInterpreter, SearchEngineRecordProcessors, SearchTools}
import com.asb.free.time.DelayUtils.{DelayInterpreter, DelayUtil, Delays}
import org.apache.commons.csv.CSVRecord

object CSVRecordsProcessorUtils {

  sealed trait CSVRecordsProcessor[A]

  case class ProcessRecord[A](csvRecord: CSVRecord, a: Option[A]) extends CSVRecordsProcessor[A]

  class CSVRecordsProcessors[F[_]](implicit I: InjectK[CSVRecordsProcessor, F]) {

    private def sequence[S[_], A](fs: Stream[Free[S, A]]): Free[S, Stream[A]] =
      fs.reverse.foldLeft[Free[S, Stream[A]]](Free.pure[S, Stream[A]](Stream()))((b, a) => map2(a, b)(_ #:: _))

    private def map2[S[_], A, B, C](ra: Free[S, A], rb: Free[S, B])(f: (A, B) => C): Free[S, C] = for {
      a <- ra
      b <- rb
    } yield f(a, b)

    def processRecord[A](csvRecord: CSVRecord): Free[F, A] =
      inject[CSVRecordsProcessor, F](ProcessRecord[A](csvRecord, None))

    def processRecords[A](csvRecords: Stream[CSVRecord]): Free[F, Stream[A]] = {
      val res = for {
        csvRecord <- csvRecords
      } yield processRecord[A](csvRecord)
      sequence[F, A](res)
    }

  }

  object CSVRecordsProcessors {
    def apply[F[_]](implicit I: InjectK[CSVRecordsProcessor, F]): CSVRecordsProcessors[F] = new CSVRecordsProcessors[F]
  }

  type SearchOrDelay[A] = EitherK[SearchTools, DelayUtil, A]

  def program(csvRecord: CSVRecord)(implicit S: SearchEngineRecordProcessors[SearchOrDelay],
                                    D: Delays[SearchOrDelay]): Free[SearchOrDelay, SEResult] = {
    import D._
    import S._
    for {
      _ <- sleep(200)
      res <- processRecord(csvRecord)
    } yield res
  }

  val interpreter: SearchOrDelay ~> Id = SearchEngineInterpreter or DelayInterpreter

  object RecordProcessorInterpreter extends (CSVRecordsProcessor ~> Id) {
    override def apply[A](fa: CSVRecordsProcessor[A]): Id[A] = fa match {
      case ProcessRecord(csvRecord, _: Option[SEResult]) =>
        implicit val searchOrDelay = SearchEngineRecordProcessors[SearchOrDelay]
        implicit val delays = Delays[SearchOrDelay]
        program(csvRecord).foldMap(interpreter)
    }
  }

}
