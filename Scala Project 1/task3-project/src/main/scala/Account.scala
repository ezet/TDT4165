import akka.actor._
import akka.event.Logging

import scala.collection.immutable.HashMap

case class TransactionRequest(toAccountNumber: String, amount: Double)

case class TransactionRequestReceipt(toAccountNumber: String,
                                     transactionId: String,
                                     transaction: Transaction)

case class BalanceRequest()

object Account {
  def props(accountId: String, bankId: String, initialBalance: Double) = Props(classOf[Account], accountId, bankId, initialBalance)
}

class Account(val accountId: String, val bankId: String, val initialBalance: Double = 0) extends Actor {

  val log = Logging(context.system, this)

  private var transactions = HashMap[String, Transaction]()

  class Balance(var amount: Double) {}

  val balance = new Balance(initialBalance)

  def getFullAddress: String = {
    bankId + accountId
  }

  def getTransactions: List[Transaction] = {
    // Should return a list of all Transaction-objects stored in transactions
    transactions.values.toList
  }

  def allTransactionsCompleted: Boolean = {
    // Should return whether all Transaction-objects in transactions are completed
    !getTransactions.exists(p => p.status == TransactionStatus.PENDING)
  }

  def withdraw(amount: Double): Unit = {
    balance.amount -= amount
  }

  def deposit(amount: Double): Unit = {
    balance.amount += amount
  }

  def getBalanceAmount: Double = {
    balance.amount
  }

  def sendTransactionToBank(t: Transaction) {
    // Should send a message containing t to the bank of this account
    BankManager.findBank(bankId) ! t
  }

  def transferTo(accountNumber: String, amount: Double): Transaction = {
    val t = new Transaction(from = getFullAddress, to = accountNumber, amount = amount)
    if (reserveTransaction(t)) {
      if (amount <= 0 || amount > getBalanceAmount) t.status = TransactionStatus.FAILED
      else {
        withdraw(amount)
        sendTransactionToBank(t)
      }
    }
    t
  }

  def reserveTransaction(t: Transaction): Boolean = {
    if (!transactions.contains(t.id)) {
      transactions += (t.id -> t)
      return true
    }
    false
  }

  override def receive = {
    case IdentifyActor => sender ! this

    // Process receipt
    case TransactionRequestReceipt(to, transactionId, transaction) =>
      if (transaction.status == TransactionStatus.FAILED) {
        deposit(transaction.amount)
      }
      transaction.receiptReceived = true


    // Should return current balance
    case BalanceRequest => sender ! getBalanceAmount

    // Handle incoming transaction
    case t: Transaction => handleTransaction(t)

    case msg => log.debug(msg.toString)

  }

  def handleTransaction(transaction: Transaction): Unit = {
    deposit(transaction.amount)
    transaction.status = TransactionStatus.SUCCESS
    sender ! TransactionRequestReceipt(transaction.from, transaction.id, transaction)
  }
}
