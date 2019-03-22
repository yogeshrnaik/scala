package stateful

class BankAccount {
  private var _balance              = 0
  private var actions: List[Action] = Nil

  def deposit(amount: Int): Unit = synchronized {
    _balance += amount
    actions ::= Deposit(amount)
    ExternalService.record(Deposit(amount))
  }

  def withdraw(amount: Int): Unit = synchronized {
    _balance -= amount
    actions ::= Withdrawal(amount)
    ExternalService.record(Withdrawal(amount))
  }

  def balance: Int = synchronized {
    _balance
  }

  def get: (Int, List[Action]) = synchronized {
    (_balance, actions)
  }
}
