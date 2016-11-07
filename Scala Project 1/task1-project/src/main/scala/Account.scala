import exceptions._

class Account(private var balance: Double, val uid: Int = Bank getUniqueId) {

  def withdraw(amount: Double): Unit = this.synchronized{
    if (amount <= 0) throw new IllegalAmountException("Amount must be greater than zero.")
    if (amount > getBalanceAmount) throw new NoSufficientFundsException()
    balance -= amount
  }// Implement

  def deposit(amount: Double): Unit = this.synchronized {
    if (amount <= 0) throw new IllegalAmountException("Amount must be greater than zero.")
    balance += amount
  } // Implement

  def getBalanceAmount: Double = {
    balance
  } // Implement
}
