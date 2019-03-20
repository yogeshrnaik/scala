package com.banking.model

abstract class Transaction {
  def description: String
  def amount : Double
  def closingBalance: Double
  def amountSign: String

  override def toString: String = description.padTo(Transaction.DESC_PADDING, " ").mkString +
    (amountSign + amount).padTo(Transaction.AMOUNT_PADDING, " ").mkString + closingBalance
}

case class Credit(val description: String, val amount: Double, val closingBalance : Double) extends Transaction {
  override def amountSign: String = "+"
}
case class Debit(val description: String, val amount: Double, val closingBalance : Double) extends Transaction {
  override def amountSign: String = "-"
}

object Transaction {
  val DESC_PADDING  = 20
  val AMOUNT_PADDING = 15
}
