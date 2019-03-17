package com.banking

import com.banking.model.BankAccount

object BankingApp {
  def main(args: Array[String]): Unit = {
    println(new BankAccount("Tom", 100000).withdraw(100000))

    println(new BankAccount("Hank", 500).deposit(200).withdraw(100))

    println(new BankAccount("John", -10))
  }
}
