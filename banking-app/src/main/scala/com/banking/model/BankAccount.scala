package com.banking.model

class BankAccount(val accountHolder: String, private var balance: Double) {

  require(balance >= 0.0, "Balance must be greater than or equal to zero")

  private val transactions : Transactions = if (balance > 0) new Transactions(Credit("Opening balance", balance, balance)) else new Transactions()

  def getBalance() = balance
  def getTransactions() = transactions

  def deposit(amount: Double) : BankAccount = synchronized {
    require(amount > 0, "Amount to be deposited must be greater than zero")
    balance = balance + amount
    transactions.add(Credit("Deposited", amount, balance))
    require(balance == transactions.reconcile)
    this
  }

  def withdraw(amount: Double) : BankAccount = synchronized {
    if (balance < amount)
      throw new InsufficientBalance(s"Balance $balance is not sufficient to withdraw $amount")
    balance = balance - amount
    transactions.add(Debit("Withdrawn", amount, balance))
    require(balance == transactions.reconcile)
    this
  }

  def contains(trans: Transaction): Boolean = synchronized { transactions.contains(trans) }

  def getLatestTransaction(): Transaction = synchronized { transactions.getLatestTransaction() }

  override def toString: String = {
    val PADDING  = 20
    "Account holder:".padTo(PADDING, " ").mkString + accountHolder + "\n" +
      "Balance:".padTo(PADDING, " ").mkString + balance + "\n" +
      transactions
  }
}

class InsufficientBalance(message: String) extends RuntimeException {
  override def toString: String = message
}