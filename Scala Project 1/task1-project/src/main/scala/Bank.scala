import exceptions.IllegalAmountException

object Bank {

  private var idCounter: Int = 0

  def transaction(from: Account, to: Account, amount: Double): Unit = {
    if (amount <= 0) throw new IllegalAmountException("Amount must be greater than zero.")
    from.withdraw(amount)
    to.deposit(amount)
  }

  def getUniqueId: Int = this.synchronized {
    idCounter += 1
    idCounter
  }

}
