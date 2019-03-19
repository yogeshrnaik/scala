package com.banking
import com.banking.model._

import collection.mutable.Stack
import org.scalatest._

class BankingAppSpec extends FlatSpec with Matchers {

  "A new BankAccount with opening balance = 0" should "not have any transactions" in {
    val acct = new BankAccount("Tom", 0)
    acct.getBalance() should be (0)
    acct.getTransactions().isEmpty() should equal (true)
  }

  "A new BankAccount" should "not get created with negative opening balance" in {
    val ex = intercept[IllegalArgumentException] {
      new BankAccount("Tom", -10)
    }
    ex.getMessage should equal ("requirement failed: Balance must be greater than or equal to zero")
  }

  "A new BankAccount with opening balance > 0" should "have latest transaction as a Credit = opening balance" in {
    val acct = new BankAccount("Tom", 100)
    acct.getBalance() should be (100)
    acct.getLatestTransaction() should equal (Credit("Opening balance", 100, 100))
  }

  "A deposit operation" should "increase balance by deposited amount and account has latest transaction as Credit with deposited amount" in {
    val acct = new BankAccount("Tom", 10)
    acct.deposit(40)

    acct.getBalance() should be (50)
    acct.getLatestTransaction() should equal (Credit(Transactions.DEPOSITED, 40, 50))
  }

  "A withdraw operation" should "decrease balance by withdrawn amount and account has latest transaction as Debit with withdrawn amount" in {
    val acct = new BankAccount("Tom", 100)
    acct.withdraw(20)

    acct.getBalance() should be (80)
    acct.getLatestTransaction() should equal (Debit(Transactions.WITHDRAWN, 20, 80))
  }

  "A withdraw operation with amount > balance" should "throw InsufficientBalance exception and not change balance" in {
    val acct = new BankAccount("Tom", 100)
    val ex = intercept[InsufficientBalance] {
      acct.withdraw(200)
    }
    ex.getMessage should equal ("Balance 100.0 is not sufficient to withdraw 200.0")
    acct.getBalance() should be (100)
  }
}
