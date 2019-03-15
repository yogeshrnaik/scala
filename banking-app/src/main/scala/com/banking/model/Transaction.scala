package com.banking.model

abstract class Transaction {
  def amount : Double
  def description: String
}

case class Credit(val amount: Double, val description: String) extends Transaction {
  override def toString: String = description + "\t" + "+" + amount
}
case class Debit(val amount: Double, val description: String) extends Transaction {
  override def toString: String = description + "\t" + "-" + amount
}