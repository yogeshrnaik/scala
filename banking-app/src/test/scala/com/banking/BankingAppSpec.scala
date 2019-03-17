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

  "A new BankAccount with opening balance > 0" should "have latest transaction as a Credit transaction equal to opening balance" in {
    val acct = new BankAccount("Tom", 10)
    acct.getBalance() should be (10)
    assert(acct.contains(Credit("Opening balance", 10, 10)) == true)
  }

  "A deposit operation" should "add a Credit transaction equal to deposited amount" in {
    val acct = new BankAccount("Tom", 10)
    acct.deposit(40)

    acct.getBalance() should be (50)
    assert(acct.getLatestTransaction() == Credit("Deposited", 40, 50))
  }

  "A withdraw operation" should "have latest transaction as a Debit transaction equal to withdrawn amount" in {
    val acct = new BankAccount("Tom", 100)
    acct.withdraw(20)

    acct.getBalance() should be (80)
    assert(acct.getLatestTransaction() == Debit("Withdrawn", 20, 80))
  }


}
