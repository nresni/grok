package com.github.nresni.rdm

import java.util.Date

import org.scalatest.{Matchers, FunSpec}

class AccountServiceSpec extends FunSpec with Matchers {

  describe("Account Service") {
    it("Credits and debits") {
      import com.github.nresni.rdm.AccountService._
      val a = Account("123", "John Doe", new Date())

      val d = for {
        b <- credit(a, 1000)
        c <- debit(b, 500)
        d <- debit(c, 300)
      } yield d

      d.get.balance should be (Balance(200))
    }

    it("Customer check succeed") {
      object StubAccountService extends AccountService
                                with Verifications { override def checkCustomer(customer: Customer): Boolean = true }

      import StubAccountService._
      val account = verifyCustomer(Customer("John doe"))
        .map(c => openCheckingAccount(c))
        .getOrElse(throw new IllegalArgumentException("Verification failed you are a fake"))

      account should be (Account("123", "John doe", account.dateOfOpening, Balance(0)))
    }

    it("Customer check failed") {
      object StubAccountService extends AccountService
                                with Verifications { override def checkCustomer(customer: Customer): Boolean = false }

      import StubAccountService._
      intercept[IllegalArgumentException] {
        verifyCustomer(Customer("John doe"))
          .map(c => openCheckingAccount(c))
          .getOrElse(throw new IllegalArgumentException("Verification failed you are a fake"))
      }
    }
  }
}
