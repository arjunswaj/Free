package com.asb.free

import java.io.{BufferedReader, BufferedWriter}
import java.nio.file.{Files, Path, Paths}

import cats.{Id, InjectK, ~>}
import cats.free.Free
import cats.free.Free.inject


object FileIOUtils {

  sealed trait FileIO[A]

  case class GetFilePath(filename: String) extends FileIO[Path]
  case class GetBufferedReader(path: Path) extends FileIO[BufferedReader]
  case class GetBufferedWriter(path: Path) extends FileIO[BufferedWriter]

  class FileIOs[F[_]](implicit I: InjectK[FileIO, F]) {

    def getFilePath(string: String): Free[F, Path] =
      inject(GetFilePath(string))

    def getBufferedReader(path: Path): Free[F, BufferedReader] =
      inject(GetBufferedReader(path))

    def getBufferedWriter(path: Path): Free[F, BufferedWriter] =
      inject(GetBufferedWriter(path))
  }

  object FileIOs {
    implicit def apply[F[_]](implicit I: InjectK[FileIO, F]): FileIOs[F] = new FileIOs[F]
  }

  object FilesInterpreter extends (FileIO ~> Id) {
    override def apply[A](fa: FileIO[A]): Id[A] = fa match {
      case GetFilePath(filename) => Paths.get(filename)
      case GetBufferedReader(path) => Files.newBufferedReader(path)
      case GetBufferedWriter(path) => Files.newBufferedWriter(path)
    }
  }

}
