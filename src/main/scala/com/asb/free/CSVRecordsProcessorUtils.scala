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

  case class ProcessRecords[A](csvRecords: Stream[CSVRecord]) extends CSVRecordsProcessor[Stream[A]]

  class CSVRecordsProcessors[F[_]](implicit I: InjectK[CSVRecordsProcessor, F]) {

    def processRecords[A](csvRecords: Stream[CSVRecord]): Free[F, Stream[A]] =
      inject[CSVRecordsProcessor, F](ProcessRecords(csvRecords))

  }

  object CSVRecordsProcessors {
    def apply[F[_]](implicit I: InjectK[CSVRecordsProcessor, F]): CSVRecordsProcessors[F] = new CSVRecordsProcessors[F]
  }

  type SearchOrDelay[A] = EitherK[SearchTools, DelayUtil, A]

  def program(implicit S: SearchEngineRecordProcessors[SearchOrDelay],
              D: Delays[SearchOrDelay], csvRecord: CSVRecord): Free[SearchOrDelay, SEResult] = {
    import S._
    import D._
    for {
      _ <- sleep(200)
      res <- processRecord(csvRecord)
    } yield res
  }

  val interpreter: SearchOrDelay ~> Id = SearchEngineInterpreter or DelayInterpreter

  object RecordProcessorInterpreter extends (CSVRecordsProcessor ~> Id) {
    override def apply[A](fa: CSVRecordsProcessor[A]): Id[A] = fa match {
      case ProcessRecords(csvRecords) =>
        implicit val searchOrDelay = SearchEngineRecordProcessors[SearchOrDelay]
        implicit val delays = Delays[SearchOrDelay]
        csvRecords.map(implicit csvRecord => program.foldMap(interpreter))
    }
  }

}
