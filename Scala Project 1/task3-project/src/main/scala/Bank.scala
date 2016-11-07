import java.util.concurrent.atomic.AtomicInteger

import akka.actor._
import akka.event.Logging
import akka.util.Timeout

import scala.concurrent.duration._

case class GetAccountRequest(accountId: String)

case class CreateAccountRequest(initialBalance: Double)

case class IdentifyActor()

object Bank {
  def props(bankId: String) = Props(classOf[Bank], bankId)
}

class Bank(val bankId: String) extends Actor {

  val accountCounter = new AtomicInteger(1000)

  val log = Logging(context.system, this)

  def createAccount(initialBalance: Double): ActorRef = {
    // Should create a new Account Actor and return its actor reference. Accounts should be assigned with unique ids (increment with 1).
    BankManager.createAccount(accountCounter.incrementAndGet().toString, bankId, initialBalance)
  }

  def findAccount(accountId: String): Option[ActorRef] = {
    // Use BankManager to look up an account with ID accountId
    Option(BankManager.findAccount(bankId, accountId))
  }

  def findOtherBank(bankId: String): Option[ActorRef] = {
    // Use BankManager to look up a different bank with ID bankId
    Option(BankManager.findBank(bankId))
  }

  override def receive = {
    case CreateAccountRequest(initialBalance) => sender ! createAccount(initialBalance) // Create a new account
    case GetAccountRequest(id) => sender ! findAccount(id) // Return account
    case IdentifyActor => sender ! this
    case t: Transaction => processTransaction(t)
    case t: TransactionRequestReceipt => forwardTransactionRequestReceipt(t)
    case msg =>
      log.debug(msg.toString)

  }

  def forwardTransactionRequestReceipt(receipt: TransactionRequestReceipt): Unit = {
    // This method should forward Transaction t to an account or another bank, depending on the "to"-address.
    // HINT: Make use of the variables that have been defined above.
    implicit val timeout = new Timeout(5 seconds)
    val to = getRef(receipt.transaction.from)
    if (to.isDefined) to.get ! receipt
    //    else {
    //      receipt.transaction.status = TransactionStatus.FAILED
    //      sender ! TransactionRequestReceipt(receipt.transaction.to, receipt.transaction.id, receipt.transaction)
    //    }
  }

  def processTransaction(transaction: Transaction): Unit = {
    // This method should forward Transaction t to an account or another bank, depending on the "to"-address.
    // HINT: Make use of the variables that have been defined above.
    implicit val timeout = new Timeout(5 seconds)
    val to = getRef(transaction.to)
    if (to.isDefined)
      to.get ! transaction
    else {
      transaction.status = TransactionStatus.FAILED
      sender ! TransactionRequestReceipt(transaction.from, transaction.id, transaction)
    }
  }

  def getRef(to: String): Option[ActorRef] = {
    val isInternal = to.length <= 4
    //    val isInternal = to.substring(0,4) == bankId
    val toBankId = if (isInternal) bankId else to.substring(0, 4)
    val toAccountId = if (isInternal) to else to.substring(4)
    if (bankId == toBankId)
      findAccount(toAccountId)
    else
      findOtherBank(toBankId)
  }
}