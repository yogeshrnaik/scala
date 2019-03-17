package com.banking
import com.banking.model.{BankAccount, Credit, Debit}

import collection.mutable.Stack
import org.scalatest._

class BankingAppSpec extends FlatSpec with Matchers {


  "A new BankAccount with opening balance = 0" should "not have any transactions" in {
    val acct = new BankAccount("Tom", 0)
    acct.getBalance() should be (0)
    assert(acct.getTransactions().isEmpty() == true)
  }

  "A new BankAccount with opening balance > 0" should "have latest transaction as a Credit = opening balance" in {
    val acct = new BankAccount("Tom", 100)
    acct.getBalance() should be (100)
    assert(acct.getLatestTransaction() == Credit("Opening balance", 100, 100))
  }

  "A deposit operation" should "have latest transaction as a Credit = deposited amount" in {
    val acct = new BankAccount("Tom", 10)
    acct.deposit(40)

    acct.getBalance() should be (50)
    assert(acct.getLatestTransaction() == Credit("Deposited", 40, 50))
  }

  "A withdraw operation" should "have latest transaction as a Debit = withdrawn amount" in {
    val acct = new BankAccount("Tom", 100)
    acct.withdraw(20)

    acct.getBalance() should be (80)
    assert(acct.getLatestTransaction() == Debit("Withdrawn", 20, 80))
  }
}
