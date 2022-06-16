//#full-example
package $package$
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, PostStop}
import akka.actor.Cancellable
import scala.util.Random
import akka.actor.typed.Signal

import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.receptionist.ServiceKey
import $package$.CborSerializable
import com.typesafe.config.ConfigFactory

import scala.concurrent.duration._
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import java.security.Provider.Service


object Killer {

  case class KillerTask(mode: String)

  def apply(serverList: ListBuffer[ListBuffer[ActorRef[Server.Command]]], supervisorLength: Int, numSupervisors: Int, killsPerSecond: Int, startIndex: Int): Behavior[KillerTask] = 
    Behaviors.setup(context => new Killer(context, serverList, supervisorLength, numSupervisors, killsPerSecond, startIndex))
}

class Killer(context: ActorContext[Killer.KillerTask], serverList: ListBuffer[ListBuffer[ActorRef[Server.Command]]], 
            supervisorLength: Int, numSupervisors: Int, killsPerSecond: Int, startIndex: Int) extends AbstractBehavior[Killer.KillerTask](context) {
  import Killer._

  var refList: ListBuffer[ListBuffer[ActorRef[Server.Command]]] = serverList
  var supLen: Int = supervisorLength
  var nSups: Int = numSupervisors

  var kps: Int = killsPerSecond
  var start: Int = startIndex
  var interval: Int = 1000/kps
  var curTask: Cancellable = null

  override def onMessage(msg: KillerTask): Behavior[KillerTask] =
    msg match {
      case KillerTask(mode) =>
        mode match {
          case "burst" =>
            var interval = 5
            curTask = context.system.scheduler.scheduleOnce(interval.second, (() => context.self ! Killer.KillerTask("burst")))

            for (i <- 1 to interval*kps) {
              killProcess()
            }

          case "stairs" =>
            curTask = context.system.scheduler.scheduleOnce(5.second, (() => context.self ! KillerTask("stairs")))
            for (i <- 1 to kps) {
              var target = Random.nextInt(serverList(start).length)
              serverList(start)(target) ! Server.Stop(0)
              serverList(start).remove(target)
              start += 1
              if (start >= nSups) {start -= nSups}
            }

          case "uniform" =>
            curTask = context.system.scheduler.scheduleOnce(interval.milli, (() => context.self ! Killer.KillerTask("uniform")))
            killProcess()
          
          case "single" =>
            killProcess()

          case "rand" =>
            var burst = false
            if (Random.nextInt(2) == 1) { burst = true }
            var size = Random.nextInt(10) + 1
            curTask = context.system.scheduler.scheduleOnce((interval*size).milli, (() => context.self ! Killer.KillerTask("rand")))

            if (burst) {
              for (i <- 1 to size) {
                killProcess()
              }
              println("Burst of " + size)
            } else {
              for (i <- 1 to size) {
                context.system.scheduler.scheduleOnce((interval*(i-1)).milli, (() => context.self ! Killer.KillerTask("single")))
              }
              println("Uniform of " + size)
            }
        }
        this
    }

  override def onSignal: PartialFunction[Signal, Behavior[Killer.KillerTask]] = {
    case PostStop =>
      if (curTask != null) {curTask.cancel()}
      this
  }

  private def killProcess(): Unit = {
    var target = Random.nextInt(supLen)           // Choose target index
    serverList(start)(target) ! Server.Stop(0)    // Send Stop message to crash Actor
    start += 1
    if (start >= nSups) {start -= nSups}
  }
}


object InjectorMain {
  val InjectorKey = ServiceKey[InjectorMain.Command]("Injector")

  sealed trait Command
  case class InjectorParams(nfails: Int, sender: ListBuffer[ListBuffer[ActorRef[Server.Command]]]) extends Command with CborSerializable
  case class StopInjector(s: String) extends Command with CborSerializable

  def apply(mode: String): Behavior[Command] =
    Behaviors.setup(context => new InjectorMain(context, mode))
}

class InjectorMain(context: ActorContext[InjectorMain.Command], mode: String) extends AbstractBehavior[InjectorMain.Command](context) {
  import InjectorMain._

  context.log.info("Registering fault injector")
  context.system.receptionist ! Receptionist.Register(InjectorKey, context.self)
  var maxKills = 5
  var killerList = new ListBuffer[ActorRef[Killer.KillerTask]]()
  var taskList = new ListBuffer[Cancellable]()

  override def onMessage(msg: Command): Behavior[Command] = 
    msg match {
      case InjectorParams(nfails, serverList) =>
        Thread.sleep(1000)
        println("Generating faultload: " + nfails + " per second, Mode: " + mode)

        var nSups = serverList.length
        var supLen = serverList(0).length
        var start = 0

        var nKillers: Int = (nfails / maxKills).toInt
        var leftover: Int = nfails % maxKills
        var spk = 1

        if (nSups > nKillers) {
          spk = (nSups / nKillers).toInt
          if ((nSups % nKillers) != 0) {
            spk += 1
          }
        }
      
        mode match {
          case "burst"  =>
            for (i <- 1 to nKillers) {
              var killer = context.spawnAnonymous(Killer(serverList, supLen, nSups, maxKills, start))
              killerList += killer
              start += spk
              if (start >= nSups) {start -= nSups}
            }

            if (leftover > 0) {
              var killer = context.spawnAnonymous(Killer(serverList, supLen, nSups, leftover, start))
              killerList += killer
            }
            
            var delay = 5000/nKillers
            var delayacc = 0
            for (k <- killerList) {
              var task = context.system.scheduler.scheduleOnce((delay + delayacc).milli, (() => k ! Killer.KillerTask("burst")))
              taskList += task
              delayacc += delay
            }
          
          case "stairs" =>
            Thread.sleep(10000)

            var parts = serverList.grouped(spk)
            for (part <- parts) {
              var killer = context.spawnAnonymous(Killer(part, 50, spk, maxKills, 0))
              killerList += killer
            }
            for (k <- killerList) {
              k ! Killer.KillerTask("stairs")
            }
            println(killerList.length)

          case "uniform" =>
            var delay: Int = (1000/maxKills)/nKillers
            if (delay < 1) {delay = 1}

            for (i <- 1 to nKillers) {
              var killer = context.spawnAnonymous(Killer(serverList, supLen, nSups, maxKills, start))
              var task = context.system.scheduler.scheduleOnce((i*delay).milli, (() => killer ! Killer.KillerTask("uniform")))
              killerList += killer
              taskList += task
              start += spk
              if (start >= nSups) {start -= nSups}
            }

            if (leftover > 0) {
              var killer = context.spawnAnonymous(Killer(serverList, supLen, nSups, leftover, start))
              var leftoverInterval = (1000/leftover).toInt
              var task = context.system.scheduler.scheduleOnce((nKillers*delay).milli, (() => killer ! Killer.KillerTask("uniform")))
              killerList += killer
              taskList += task
            }
            
          case "rand" =>
            var delay = (1000/maxKills)/nKillers
            if (delay < 1) {delay = 1}
            var delayacc = 0
            
            for (i <- 1 to nKillers) {
              delayacc += Random.nextInt(delay)

              var killer = context.spawnAnonymous(Killer(serverList, supLen, nSups, maxKills, start))
              var task = context.system.scheduler.scheduleOnce(delayacc.milli, (() => killer ! Killer.KillerTask("rand")))
              killerList += killer
              taskList += task
              start += spk
              if (start >= nSups) {start -= nSups}
            }
            
          case _ =>
            println("Unknown faultload pattern")
        }
        println(killerList.length + " killers spawned")
        this

      case StopInjector(s) =>
        for (i <- 1 to killerList.length) {
          context.stop(killerList(i-1))
          taskList(i-1).cancel()
        }
        killerList = new ListBuffer[ActorRef[Killer.KillerTask]]()
        taskList = new ListBuffer[Cancellable]()
        this
    }
}