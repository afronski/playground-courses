package week5

import akka.actor.Actor
import akka.actor.Props

import akka.event.LoggingReceive

class TransferMain extends Actor {
  val accountFrom = context.actorOf(Props[BankAccount], "accountFrom")
  val accountTo = context.actorOf(Props[BankAccount], "accountTo")

  accountFrom ! BankAccount.Deposit(100)
  accountFrom ! BankAccount.Deposit(100)

  def receive = {
    case BankAccount.Done => transfer(150)
  }

  def transfer(amount: BigInt): Unit = {
    val transaction = context.actorOf(Props[WireTransfer], "transfer")

    transaction ! WireTransfer.Transfer(accountFrom, accountTo, amount)

    context.become(LoggingReceive {
      case WireTransfer.Done => {
        println("Transfer succeeded.")
        context.stop(self)
      }

      case WireTransfer.Failed => {
        println("Transfer failed.")
        context.stop(self)
      }
    })
  }
}