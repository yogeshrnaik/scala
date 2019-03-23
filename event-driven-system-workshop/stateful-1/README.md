

Setup
------

- Install JDK 11
- Install latest sbt (1.2.8)
- Install latest IntelliJ (2018.3.5)
- Install latest Scala plugin from within IntelliJ
- Install git
- clone this repo
- cd into the cloned repo
- run "sbt update" (this will take a long time, please be patient)
- From IntelliJ, "open" this project, select options to download sources (this will take a long time, wait till indexing finishes)


Workshop Notes
------------------
Actor based implementation

Define protocol first. That is the messages that Actor can send/receive.

```scala
trait Action
case class Deposit(amount: Int) extends Action
case class Withdrawal(amount: Int) extends Action
case class GetBalance() extends Action
```

Then define the behavior to handle each incoming message type.

E.g. What to do when we get Deposit or Withdrawal message.

Once behavior is defined, we need to spawn the behavior using Akka ActorSystem.

Once we spawn the behavior, it gives us ActorRef using which we can send message / invoke the behavior.

#### Active Object vs Actor
In Active Object, there is a queue and a single thread processing the items from queue one at a time.

In Actor, the same thing is done by Akka framework. 

But, Akka framework can choose to schedule the processing of each message on different thread.
