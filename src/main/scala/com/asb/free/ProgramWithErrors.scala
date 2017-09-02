package com.asb.free

import cats.data.EitherK
import cats.free.Free
import cats.free.Free.inject
import cats.implicits._
import cats.{InjectK, ~>}

/**
  * Program With Errors.
  * Created by arjun on 9/1/17.
  */
object ProgramWithErrors {

  case class Employee(name: String, managerId: Long, id: Long)
  case class Address(line1: String, line2: String, country: String)

  type ErrorOr[A] = Either[Exception, A]

  sealed trait EmployeeAction[R]
  case class GetEmployee(id: Long) extends EmployeeAction[Employee]
  case class EmployeeIdByName(name: String) extends EmployeeAction[Long]

  class EmployeeActions[F[_]](implicit I: InjectK[EmployeeAction, F]) {
    type GetEmployeeF[A] = Free[F, A]

    def getEmployeeIdByName(name: String): Free[F, Long] =
      inject(EmployeeIdByName(name))

    def getEmployee(id: Long): GetEmployeeF[Employee] =
      inject(GetEmployee(id))
  }
  object EmployeeActions {
    def apply[F[_]](implicit I: InjectK[EmployeeAction, F]): EmployeeActions[F] = new EmployeeActions[F]
  }


  object EmployeeInterpreter extends (EmployeeAction ~> ErrorOr) {
    override def apply[A](fa: EmployeeAction[A]): ErrorOr[A] = fa match {
      case GetEmployee(id) =>
        if (id == 1L) Right(Employee("Arjun", 11L, 1L))
        else if (id == 2L) Right(Employee("Aditya", 11L, 2L))
        else Left(new Exception("No such employee exists"))
      case EmployeeIdByName(name) =>
        name match {
          case "Arjun" => Right(1L)
          case "Aditya" => Right(2L)
          case _ => Right(0L)
        }
    }
  }

  sealed trait ContactAction[R]
  case class GetAddress(empId: Long) extends ContactAction[Address]

  class ContactActions[F[_]](implicit I: InjectK[ContactAction, F]) {
    type GetAddressF[A] = Free[F, A]

    def getAddress(id: Long): GetAddressF[Address] =
      inject(GetAddress(id))
  }
  object ContactActions {
    def apply[F[_]](implicit I: InjectK[ContactAction, F]): ContactActions[F] = new ContactActions[F]
  }

  object ContactInterpreter extends (ContactAction ~> ErrorOr) {
    override def apply[A](fa: ContactAction[A]): ErrorOr[A] = fa match {
      case GetAddress(id) =>
        if (id == 1L) Right(Address("Vijayanagar", "Bangalore", "India"))
        else if (id == 11L) Right(Address("CP", "Delhi", "India"))
        else Left(new Exception("No Address exists"))
    }
  }

  type Program[A] = EitherK[EmployeeAction, ContactAction, A]

  def findManagerAddress(name: String)
                        (implicit EA: EmployeeActions[Program],
                         CA: ContactActions[Program]): Free[Program, Address] = {
    import CA._
    import EA._
    val address: Free[Program, Address] = for {
      employeeId <- getEmployeeIdByName(name)
      employee <- getEmployee(employeeId)
      managerAddress <- getAddress(employee.managerId)
    } yield managerAddress
    address
  }

  def main(args: Array[String]): Unit = {
    implicit val EA: EmployeeActions[Program] = EmployeeActions[Program]
    implicit val CA: ContactActions[Program] = ContactActions[Program]
    val interpreter: Program ~> ErrorOr = EmployeeInterpreter or ContactInterpreter
    val res = findManagerAddress("Arjun").foldMap(interpreter)
    println(res)
  }

}