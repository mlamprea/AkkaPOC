/**
  * Created by mlamprea on 26/11/16.
  */

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.routing.RoundRobinPool


trait PiMessage

case object Calculate extends PiMessage

case class Work(start: Int, nrOfElements: Int) extends PiMessage

case class Result(value: Double) extends PiMessage

case class PiApproximation(pi: Double, duration: Long)

class Worker extends Actor {
  def receive: Receive = {
    case Work(start, nrOfElements) => sender ! Result(calculatePiFor(start, nrOfElements))
  }

  def calculatePiFor(start: Int, nrOfElements: Int): Double = {
    var acc = 0.0
    for (i <- start until (start + nrOfElements))
      acc += 4.0 * (1 - (i % 2) * 2) / (2 * i + 1)
    acc
  }
}

class Master(nrOfWorkers: Int, nrOfMessages: Int, nrOfElements: Int, listener: ActorRef) extends Actor {
  var pi: Double = _
  var nrOfResults: Int = _
  val start: Long = System.currentTimeMillis()

  val workerRouter: ActorRef =
    context.actorOf(RoundRobinPool(nrOfWorkers).props(Props[Worker]), "router")

  def receive = {
    case Calculate ⇒
      for (i ← 0 until nrOfMessages) workerRouter ! Work(i * nrOfElements, nrOfElements)
    case Result(value) ⇒
      pi += value
      nrOfResults += 1
      if (nrOfResults == nrOfMessages) {
        // Send the result to the listener
        listener ! PiApproximation(pi, duration = System.currentTimeMillis - start)
        // Stops this actor and all its supervised children
        context.stop(self)
      }
  }
}

class Listener extends Actor {
  def receive() = {
    case PiApproximation(pi, duration) => println("Pi = %s\nTime= %s".format(pi, duration))
      context.system.terminate()
  }
}

object Pi extends App {
  def calculate(nrOfWorkers: Int, nrOfMessages: Int, nrOfElements: Int) = {
    val actorSystem = ActorSystem("PiSystem")
    val listener = actorSystem.actorOf(Props[Listener], name = "listener")
    val master = actorSystem.actorOf(Props(new Master(nrOfWorkers, nrOfMessages, nrOfElements, listener)))
    master ! Calculate
  }

  calculate(args(0) toInt, args(1) toInt, args(2) toInt)
}


