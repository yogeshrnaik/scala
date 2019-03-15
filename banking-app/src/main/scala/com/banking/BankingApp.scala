package com.banking

import com.banking.model.BankAccount

object BankingApp {
  def main(args: Array[String]): Unit = {
    var acct: BankAccount = new BankAccount("Tom", 10.0)
      .withdraw(10)
      .deposit(20)
      .withdraw(10)
      .withdraw(10)
    println(acct)
  }
}
