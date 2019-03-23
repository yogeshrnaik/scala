package stateful

import java.util.concurrent.Executors

import akka.actor.ActorSystem
import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.adapter.UntypedActorSystemOps
import stateful.BankAccountTest.{bankAccount, finalFuture}

import scala.async.Async.{async, await}
import scala.concurrent.{ExecutionContext, Future}

object BankAccount2 {

  def behaviour(balance: Int): Behavior[Action] = Behaviors.receiveMessage[Action] { action =>
    action match {
      case Deposit(amount) =>
        println(s"deposting $amount")
        behaviour(balance + amount)
      case Withdrawal(amount) =>
        println(s"withdrawing $amount")
        behaviour(balance - amount)
      case GetBalance() =>
        println(s"current balance is $balance")
        Behaviors.same
    }
  }
}

object Main extends App {
  val actorSystem                = ActorSystem("test")
  val actorRef: ActorRef[Action] = actorSystem.spawnAnonymous(BankAccount2.behaviour(0))

  implicit val ec: ExecutionContext = {
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(100))
  }

  Future.traverse((1 to 10000).toList) { x =>
    actorRef ! Deposit(10)
    actorRef ! Withdrawal(10)
    Future.unit
  }

  val finalFuture = Future.traverse((1 to 10000).toList) { x =>
    val f1 = async {
      await(Future.unit.map(_ => actorRef ! Deposit(10)))
    }
    val f2 = async {
      await(Future.unit.map(_ => actorRef ! Withdrawal(10)))
    }
    async {
      await(f1)
      await(f2)
    }
  }

  async {
    await(finalFuture)
    val balance = await(Future.unit.map(_ => actorRef ! GetBalance()))
    println(balance)
  }
}
