package com.asb.free

import java.io.{BufferedReader, BufferedWriter, Reader, Writer}
import java.nio.file.{Files, Path, Paths}

import cats.free.Free
import cats.free.Free.inject
import cats.{Id, InjectK, ~>}


object FileIOUtils {

  sealed trait FileIO[A]

  case class GetFilePath(filename: String) extends FileIO[Path]

  case class GetBufferedReader(path: Path) extends FileIO[BufferedReader]

  case class CloseReader(reader: Reader) extends FileIO[Unit]

  case class GetBufferedWriter(path: Path) extends FileIO[BufferedWriter]

  case class CloseWriter(writer: Writer) extends FileIO[Unit]

  class FileIOs[F[_]](implicit I: InjectK[FileIO, F]) {

    def getFilePath(string: String): Free[F, Path] =
      inject(GetFilePath(string))

    def getBufferedReader(path: Path): Free[F, BufferedReader] =
      inject(GetBufferedReader(path))

    def closeReader(reader: Reader): Free[F, Unit] =
      inject(CloseReader(reader))

    def getBufferedWriter(path: Path): Free[F, BufferedWriter] =
      inject(GetBufferedWriter(path))

    def closeWriter(writer: Writer): Free[F, Unit] =
      inject(CloseWriter(writer))
  }

  object FileIOs {
    implicit def apply[F[_]](implicit I: InjectK[FileIO, F]): FileIOs[F] = new FileIOs[F]
  }

  object FilesInterpreter extends (FileIO ~> Id) {
    override def apply[A](fa: FileIO[A]): Id[A] = fa match {
      case GetFilePath(filename) => Paths.get(filename)
      case GetBufferedReader(path) => Files.newBufferedReader(path)
      case CloseReader(reader) => reader.close()
      case GetBufferedWriter(path) => Files.newBufferedWriter(path)
      case CloseWriter(writer) => writer.close()
    }
  }

}
