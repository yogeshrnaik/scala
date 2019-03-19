package com.banking.model

class Transactions(val transaction: Transaction) {

  private var transactions: List[Transaction] = if (transaction != null) List(transaction) else List.empty

  def this() {
    this(null)
  }

  def isEmpty() = transactions == null || transactions.size == 0

  def add(transaction: Transaction) = transactions = transaction :: transactions

  def reconcile : Double = transactions.foldLeft(0.0)((cumulativeBalance, t) => t match {
    case Credit(_, amount, _) => cumulativeBalance + amount
    case Debit(_, amount, _) => cumulativeBalance - amount
  })

  def contains(tran: Transaction): Boolean = transactions.contains(tran)


  def getLatestTransaction(): Transaction = if (isEmpty()) null else transactions.head

  override def toString: String = {
    var result = "Transactions:\n===============\n" +
      "Description".padTo(Transaction.DESC_PADDING, " ").mkString +
      "Amount".padTo(Transaction.AMOUNT_PADDING, " ").mkString +
      "Closing Balance" + "\n"
    transactions.foreach(t => result = result + t + "\n")
    result
  }
}

object Transactions {
  val DEPOSITED = "Deposited"
  val WITHDRAWN = "Withdrawn"
}