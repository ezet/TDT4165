import exceptions.{IllegalAmountException, NoSufficientFundsException}

import scala.concurrent.ExecutionContext
import scala.concurrent.forkjoin.ForkJoinPool

class Bank(val allowedAttempts: Integer = 3) {

  private var accountIdCounter = 0

  private val uid = 1
   val transactionsQueue: TransactionQueue = new TransactionQueue()
   val processedTransactions: TransactionQueue = new TransactionQueue()
  private val executionContext = ExecutionContext.fromExecutorService(new ForkJoinPool())

  executionContext.execute(new Runnable {
    override def run(): Unit = processTransactions()
  })

  def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
    transactionsQueue.push(new Transaction(
      transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
    )
  }

  private def processTransactions(): Unit = {
    while(true) {
      val transaction = transactionsQueue.pop
      try {
        transaction.run()
        transaction.status = TransactionStatus.SUCCESS
      } catch {
        case e: IllegalAmountException => {
          transaction.status = TransactionStatus.FAILED
        }
        case e: NoSufficientFundsException => {
          transaction.allowedAttemps -= 1
          if (transaction.allowedAttemps == 0) {
            transaction.status = TransactionStatus.FAILED
          }
          else {
            transactionsQueue.push(transaction)
          }
        }
      } finally {
        if (transaction.status != TransactionStatus.PENDING) processedTransactions.push(transaction)
      }
    }
  }

  def addAccount(initialBalance: Double): Account = {
    new Account(this, initialBalance)
  }

  def getProcessedTransactionsAsList: List[Transaction] = {
    processedTransactions.iterator.toList
  }

  def generateAccountId: Int = this.synchronized {
    accountIdCounter += 1
    accountIdCounter
  }

}
