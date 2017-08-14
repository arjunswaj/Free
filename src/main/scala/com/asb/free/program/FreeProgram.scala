package com.asb.free.program

import cats.data.EitherK
import cats.free.Free
import cats.{Id, ~>}
import com.asb.free.CSVRecordsProcessorUtils.{CSVRecordsProcessor, CSVRecordsProcessors, RecordProcessorInterpreter}
import com.asb.free.CSVUtils.{CSVIO, CSVIOs, CSVInterpreter}
import com.asb.free.FileIOUtils.{FileIO, FileIOs, FilesInterpreter}
import com.asb.free.SearchEngineUtil.SEResult

object FreeProgram {

  type FilesOrCSV[A] = EitherK[FileIO, CSVIO, A]
  type Script[A] = EitherK[CSVRecordsProcessor, FilesOrCSV, A]

  val filesOrCsvInterpreter: FilesOrCSV ~> Id = FilesInterpreter or CSVInterpreter
  val intepreter: Script ~> Id = RecordProcessorInterpreter or filesOrCsvInterpreter

  def program[A] (fileName: String)(implicit F: FileIOs[Script], CI: CSVIOs[Script],
              C: CSVRecordsProcessors[Script]): Free[Script, Stream[A]] = {
    import C._
    import CI._
    import F._
    for {
      path <- getFilePath(fileName)
      reader <- getBufferedReader(path)
      records <- readCSV(reader)
      result <- processRecords[A](records)
      _ <- closeReader(reader)
    } yield result
  }

  def main(args: Array[String]): Unit = {
    implicit val C = CSVRecordsProcessors[Script]
    val result = program[SEResult]("asb.csv").foldMap(intepreter)
    result.foreach(k => println(k))
  }

}
