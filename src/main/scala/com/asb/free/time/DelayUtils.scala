package com.asb.free.time

import cats.free.Free
import cats.free.Free.inject
import cats.{Id, InjectK, ~>}

object DelayUtils {

  sealed trait DelayUtil[A]

  case class Sleep(ms: Long) extends DelayUtil[Unit]

  class Delays[F[_]](implicit I: InjectK[DelayUtil, F]) {

    def sleep(ms: Long): Free[F, Unit] =
      inject(Sleep(ms))

  }

  object Delays {
    def apply[F[_]](implicit I: InjectK[DelayUtil, F]): Delays[F] = new Delays[F]
  }

  object DelayInterpreter extends (DelayUtil ~> Id) {
    override def apply[A](fa: DelayUtil[A]): Id[A] = fa match {
      case Sleep(ms) => try {
        Thread.sleep(ms)
      } catch {
        case ex: Exception => ()
      }
    }
  }

}
