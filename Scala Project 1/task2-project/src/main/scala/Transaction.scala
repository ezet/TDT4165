import java.util.concurrent._

import exceptions._

import scala.collection.mutable


object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {

  val transactions = mutable.Queue[Transaction]()
//  private val t = java.util.concurrent.BlockingQueue[Transaction]()

  // Remove and return the first element from the queue
  def pop: Transaction = this.synchronized {
    while (transactions.isEmpty) {
      this.wait()
    }
    transactions.dequeue
  }

  // Return whether the queue is empty
  def isEmpty: Boolean = transactions.isEmpty

  // Add new element to the back of the queue
  def push(t: Transaction): Unit =  this.synchronized {
    transactions.enqueue(t)
    this.notifyAll()
  }

  // Return the first element from the queue without removing it
//  def peek: Transaction = transactions.peek

  // Return an iterator to allow you to iterate over the queue
  def iterator: Iterator[Transaction] = transactions.toIterator
}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  var allowedAttemps: Int) extends Runnable {

  var status: TransactionStatus.Value = TransactionStatus.PENDING

  override def run: Unit = {

    def doTransaction() = {
      from withdraw amount
      to deposit amount
    }

    if (from.uid < to.uid) from.synchronized {
      to.synchronized {
        doTransaction
      }
    } else to.synchronized {
      from.synchronized {
        doTransaction
      }
    }

    // Extend this method to satisfy new requirements.

  }
}
