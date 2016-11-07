import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

  class Balance(var amount: Double) {}

  val balance = new Balance(initialBalance)
  val uid = bank.generateAccountId

  def withdraw(amount: Double): Unit = this.synchronized {
    if (amount <= 0) throw new IllegalAmountException("Amount must be greater than zero.")
    if (amount > getBalanceAmount) throw new NoSufficientFundsException()
    balance.amount -= amount
  } // Like in part 1

  def deposit(amount: Double): Unit = this.synchronized {
    if (amount <= 0) throw new IllegalAmountException("Amount must be greater than zero.")
    balance.amount += amount
  } // Like in part 1

  def getBalanceAmount: Double = {
    balance.amount
  } // Like in part 1

  def transferTo(account: Account, amount: Double) = {
    bank.addTransactionToQueue(this, account, amount)
  }


}
