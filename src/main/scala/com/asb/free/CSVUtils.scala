package com.asb.free

import java.io.{Reader, Writer}

import cats.free.Free
import cats.free.Free.inject
import cats.{Id, InjectK, ~>}
import org.apache.commons.csv.{CSVFormat, CSVPrinter, CSVRecord}

object CSVUtils {

  sealed trait CSVIO[A]

  case class ReadCSV(reader: Reader) extends CSVIO[Stream[CSVRecord]]

  case class WriteCSV(writer: Writer, records: Stream[Iterable[String]]) extends CSVIO[Unit]

  class CSVIOs[F[_]](implicit I: InjectK[CSVIO, F]) {

    def readCSV(reader: Reader): Free[F, Stream[CSVRecord]] =
      inject(ReadCSV(reader))

    def writeCSV(writer: Writer, records: Stream[Iterable[String]]): Free[F, Unit] =
      inject(WriteCSV(writer, records))

  }

  object CSVIOs {
    implicit def apply[F[_]](implicit I: InjectK[CSVIO, F]): CSVIOs[F] = new CSVIOs[F]
  }

  object CSVInterpreter extends (CSVIO ~> Id) {
    override def apply[A](fa: CSVIO[A]): Id[A] = fa match {
      case ReadCSV(reader) => CSVOperations.readCSV(reader)
      case WriteCSV(writer, records) => CSVOperations.writeCSV(writer, records)
    }
  }

  object CSVOperations {

    import scala.collection.JavaConverters._

    def readCSV(reader: Reader): Id[Stream[CSVRecord]] = {
      try {
        CSVFormat.RFC4180
          .withFirstRecordAsHeader()
          .parse(reader)
          .getRecords.iterator().asScala.toStream
      } catch {
        case ex: Exception => Stream.empty[CSVRecord]
      }
    }

    def writeCSV(writer: Writer, records: Stream[Iterable[String]]): Id[Unit] = {
      try {
        val csvFileFormat = CSVFormat.DEFAULT.withRecordSeparator("\n")
        val csvFilePrinter = new CSVPrinter(writer, csvFileFormat)
        records.foreach(record => csvFilePrinter.printRecord(record))
      } catch {
        case ex: Exception => ()
      }
    }

  }

}
