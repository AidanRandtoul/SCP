//#full-example
package $package$
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, PostStop}
import akka.actor.ActorKilledException
import akka.actor.Kill
import java.io._
import akka.actor.typed.Signal


import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.receptionist.Receptionist.{Find, Listing}
import $package$.CborSerializable
import com.typesafe.config.ConfigFactory


import scala.concurrent.duration._
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import akka.actor.typed.SupervisorStrategy
import scala.util.{Failure, Random, Success}
import akka.util.Timeout
import akka.actor.Cancellable


object Client {
    def apply(): Behavior[Server.Ping] =
        Behaviors.setup(context => new Client(context))
}

class Client(context: ActorContext[Server.Ping]) extends AbstractBehavior[Server.Ping](context) {

    var contents: Array[Byte] = null
    var reply: Cancellable = null

    override def onMessage(msg: Server.Ping): Behavior[Server.Ping] = {
        msg match {
            case Server.Ping(data, replyTo, time) =>
                contents = data.clone()
                reply = context.system.scheduler.scheduleOnce(175.milli, (() => replyTo ! Server.Pong(contents, time)))
                this
        }
    }

    override def onSignal: PartialFunction[Signal, Behavior[Server.Ping]] = {
        case PostStop =>
            if (reply != null) {reply.cancel()}
            this
    }
}

object Server {
    sealed trait Command

    case class Ping(data: Array[Byte], replyTo: ActorRef[Server.Command], time: Long) extends Command
    case class Pong(data: Array[Byte], time: Long) extends Command
    case class StartServer(data: Array[Byte]) extends Command
    case class Stop(i: Int) extends Command with CborSerializable

    def apply(counter: ActorRef[Counter.Command]): Behavior[Server.Command] = 
        Behaviors.setup(context => new Server(context, counter))
}

class Server(context: ActorContext[Server.Command], manager: ActorRef[Counter.Command]) extends AbstractBehavior[Server.Command](context) {
    import Server._
    
    var sendTime: Long  = 0
    var client = context.spawn(Client(), "client")
    var contents: Array[Byte] = null
    context.self ! StartServer(Array.fill[Byte](500)(1))

    override def onMessage(msg: Server.Command): Behavior[Server.Command] = {
        msg match {
            case Stop(i) =>
                var n = 1 / i
                this
            case StartServer(data) =>
                sendTime = System.currentTimeMillis()
                client ! Ping(data, context.self, sendTime)
                this
            case Pong(data, time) =>
                if (time == sendTime) {
                    contents = data.clone()
                    sendTime = System.currentTimeMillis()
                    client ! Ping(contents, context.self, sendTime)
                    manager ! Counter.Aggregate(1)
                }
                this
            case _ =>
                println("Ping recieved, incorrect message type!")
                this
        }
    }
}

object Counter {
    sealed trait Command

    case class StartCounter(s: String) extends Command
    case class Aggregate(i: Int) extends Command
    case class TakeReading(s: String) extends Command
    case class EndCounter(mode: String) extends Command

    def apply(): Behavior[Counter.Command] = 
        Behaviors.setup(context => new Counter(context)) 
}

class Counter(context: ActorContext[Counter.Command]) extends AbstractBehavior[Counter.Command](context) {
    import Counter._

    var aggregate: Int = 0
    var t00: Long = 0
    var record = new ListBuffer[Long]()

    override def onMessage(msg: Counter.Command): Behavior[Counter.Command] = {
        msg match {
            case StartCounter(s) =>
                t00 = System.currentTimeMillis()
                aggregate = 0
                record = new ListBuffer[Long]()
                this
            case Aggregate(i) =>
                aggregate += i
                this
            case TakeReading(s) =>
                var throughput = (aggregate / ((System.currentTimeMillis()-t00)/1000.000)).round
                record += throughput
                aggregate = 0
                t00 = System.currentTimeMillis()
                this
            case EndCounter(mode) =>
                var finalRecord = record.toList
                mode match {
                    case "Avg" =>
                        var average = finalRecord.sum / finalRecord.length
                        println("Average throughput: " + average)
                        val bw = new BufferedWriter(new FileWriter(new File("log.txt"), true))
                        bw.write(average.toString + "\n")
                        bw.close
                        this
                    case "Full" =>
                        val bw = new BufferedWriter(new FileWriter(new File("log.txt"), true))
                        val data = record.toList
                        for (d <- data) {
                            bw.write(d.toString + "\n")
                        }
                        bw.close
                        this
                    case _ =>
                        println("Unknown print type requested!")
                        this
                }
        }
    }
}

object HeadSupervisor {

    sealed trait Command
    case class StartSystem(s: String) extends Command
    case class FindInjector(s: String) extends Command
    case class ChildList(children: ListBuffer[ActorRef[Server.Command]]) extends Command

    def apply(num: Int, size: Int, nfails: Float): Behavior[Command] =
        Behaviors.setup(context => new HeadSupervisor(context, num, size, nfails))
}

class HeadSupervisor(context: ActorContext[HeadSupervisor.Command], num: Int, size: Int, nfails: Float) extends AbstractBehavior[HeadSupervisor.Command](context) {
    import HeadSupervisor._

    var counter = context.spawn(Counter(), "counter")
    var supervisors = new ListBuffer[ActorRef[SubSupervisor.StartSupervisor]]()
    for (a <- 1 to num) {
        var name: String = "supervisor".concat(a.toString)
        supervisors += context.spawn(SubSupervisor(size, counter, context.self), name)
    }
    var supervisorList = supervisors.toList
    var injector: Option[ActorRef[InjectorMain.Command]] = None
    var servers = new ListBuffer[ListBuffer[ActorRef[Server.Command]]]()

    override def onMessage(msg: Command): Behavior[Command] =
        msg match {
            case ChildList(children) =>
                servers += children
                this

            case StartSystem(s) =>
                
                var numpairs = num*size
                println("Starting system with " + numpairs + " pairs across " + num + " supervisors")
                
                //for (supervisor <- supervisorList) supervisor ! SubSupervisor.StartSupervisor("Go!")
                context.system.scheduler.scheduleOnce(5.second, (() => counter ! Counter.StartCounter("Go!")))

                context.system.scheduler.scheduleAtFixedRate(6.second, 1.second)(() => counter ! Counter.TakeReading("Update"))
                
                context.system.scheduler.scheduleOnce(230.second, (() => counter ! Counter.EndCounter("Full")))

                injector.foreach { i =>
                    context.system.scheduler.scheduleOnce(240.second, (() => i ! InjectorMain.StopInjector("")))
                }
                this

            case FindInjector(s) =>
                
                Thread.sleep(5000)
                implicit val timeout: Timeout = 5.second
                context.ask(
                    context.system.receptionist,
                    Find(InjectorMain.InjectorKey)
                ) {
                    case Success(listing: Listing) =>
                        var instances: Set[ActorRef[InjectorMain.Command]] = listing.serviceInstances(InjectorMain.InjectorKey)

                        injector = instances.headOption
                        println(injector)
                        if (injector == None) {
                            FindInjector("")
                        }
                        injector.foreach { i =>
                            if (nfails != 0) {
                                var nRequests = num*size // Each process pair completes roughly 4.6 exchanges per second
                                i ! InjectorMain.InjectorParams((nRequests*(nfails/100)).toInt, servers)
                            }
                        }    
                        StartSystem("")
                    case Failure(_) =>
                        println("No injector found!")
                        FindInjector("")
                }
                this
        }
}

object SubSupervisor {
    case class StartSupervisor(s: String)

    def apply(num: Int, counter: ActorRef[Counter.Command], parent: ActorRef[HeadSupervisor.Command]): Behavior[StartSupervisor] =
        Behaviors.setup(context => new SubSupervisor(context, num, counter, parent))
}

class SubSupervisor(context: ActorContext[SubSupervisor.StartSupervisor], num: Int, counter: ActorRef[Counter.Command], parent: ActorRef[HeadSupervisor.Command]) extends AbstractBehavior[SubSupervisor.StartSupervisor](context) {
    import SubSupervisor._

    var servers = new ListBuffer[ActorRef[Server.Command]]()
    for (a <- 1 to num) {
        var name: String = "server".concat(a.toString)
        servers += context.spawn(
            Behaviors.supervise(Server(counter)).onFailure[ArithmeticException](SupervisorStrategy.stop), name)
    }
    var serverList = servers.toList
    parent ! HeadSupervisor.ChildList(servers)
    var data = Array.fill[Byte](500)(1)

    override def onMessage(msg: StartSupervisor): Behavior[StartSupervisor] =
        msg match {
            case StartSupervisor(s) =>
                for (server <- serverList) server ! Server.StartServer(data)
                this
        }
}


object SCPMain {
    case class SystemParams(numsups: Int, supsize: Int, nfails: Float)

    def apply(): Behavior[SystemParams] =
        Behaviors.setup(context => new SCPMain(context))
}

class SCPMain(context: ActorContext[SCPMain.SystemParams]) extends AbstractBehavior[SCPMain.SystemParams](context) {
    import SCPMain._
    //context.spawn(ClusterListener(), "ClusterListener")

    override def onMessage(msg: SystemParams): Behavior[SystemParams] =
        msg match {
            case SystemParams(n, s, f) =>

                var headSup = context.spawn(HeadSupervisor(n, s, f), "HeadSupervisor")
                Thread.sleep(2000)

                headSup ! HeadSupervisor.FindInjector("Start!")
                this
            }
}