package com.banking.model

class BankAccount(accountHolder: String, balance: Double) {
  require(balance >= 0.0)
  private var transactions: List[Transaction] = List(Credit(balance, "Opening balance"))

  private def this(accountHolder: String, openingBalance: Double, transactions: List[Transaction]) {
    this(accountHolder, openingBalance)
    require(openingBalance >= 0.0)
    this.transactions = transactions;
  }

  def currentBalance : Double = transactions.foldLeft(0.0)((cumulativeBalance, t) => t match {
        case Credit(amount, _) => cumulativeBalance + amount
        case Debit(amount, _) => cumulativeBalance - amount
    })

  def deposit(amount: Double) : BankAccount = {
      new BankAccount(this.accountHolder, this.balance,  new Credit(amount, "Credited") :: transactions)
  }

  def withdraw(amount: Double) : BankAccount = {
    if (this.currentBalance < amount)
      throw new InsufficientBalance("Balance is not sufficient.")
    new BankAccount(this.accountHolder, this.balance, new Debit(amount, "Withdrawn") :: transactions)
  }

  override def toString: String = {
    var result = "Account holder: " + accountHolder + "\n" + "Balance: " + currentBalance + "\n" + "Transactions:\n"
    transactions.foreach(t => result = result + t + "\n")
    result
  }
}

class InsufficientBalance(message: String) extends RuntimeException