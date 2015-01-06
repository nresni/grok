package com.github.nresni.rdm

import java.util.Date

import com.github.nresni.rdm.AccountTypes.Amount

import scala.util.{Success, Failure, Try}

object AccountTypes { type Amount = BigDecimal }

case class Customer(name: String)

case class Balance(amount: Amount = 0)

case class Account(no: String, name: String, dateOfOpening: Date, balance: Balance = Balance())

trait AuditLog {
  def generateAuditLog: (Account, Amount) => Try[String] = (_, _) => Success("Plop")
  def write: String => Unit = println
}

trait Verifications {
  def checkCustomer(customer: Customer): Boolean
}

trait VerificationsService extends Verifications {
  def checkCustomer(customer: Customer): Boolean = ???
}

trait AccountService {
  this: Verifications =>

  def openCheckingAccount(customer: Customer): Account = Account("123", customer.name, new Date())

  def verifyCustomer(customer: Customer): Option[Customer] = {
    if (checkCustomer(customer)) Option(customer)
    else None
  }

  def debit(a: Account, amount: Amount): Try[Account] = {
    if (a.balance.amount < amount) Failure(new Exception("No money !"))
    else Success(a.copy(balance = Balance(a.balance.amount - amount)))
  }

  def credit(a: Account, amount: Amount): Try[Account] =
    Success(a.copy(balance = Balance(a.balance.amount + amount)))
}

object AccountService extends AccountService with VerificationsService

object AccountDemoApp extends App with AuditLog {
  import com.github.nresni.rdm.AccountService._
  val a = Account("123", "John Doe", new Date(), Balance(1000))
  val amount = 200
  debit(a, amount)
    .flatMap(b => generateAuditLog(b, amount))
    .foreach(write)
}