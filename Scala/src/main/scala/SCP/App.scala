package $package$

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import com.typesafe.config.ConfigFactory
import java.io._

object App {

    def main(args: Array[String]): Unit = {
        var job = args(0)

        job match {
            case "inject" =>
                var mode = args(1)
                val config = ConfigFactory.parseString(s"""
                akka.remote.artery.canonical.hostname=130.209.255.1
                akka.remote.artery.canonical.port=25251
                """).withFallback(ConfigFactory.load())

                val system: ActorSystem[InjectorMain.InjectorParams] = ActorSystem(InjectorMain(mode), "Cluster", config)
            case "process" =>
                val config = ConfigFactory.parseString(s"""
                akka.remote.artery.canonical.hostname=130.209.255.2
                akka.remote.artery.canonical.port=25252
                """).withFallback(ConfigFactory.load())

                val system: ActorSystem[SCPMain.SystemParams] = ActorSystem(SCPMain(), "Cluster", config)
                system ! SCPMain.SystemParams(args(1).toInt, args(2).toInt, args(3).toFloat)

                val bw = new BufferedWriter(new FileWriter(new File("log.txt"), true))

                if (args(3) == "0" ){
                    bw.write("Starting run: NSups - " + args(1) + ", SupSize - " + args(2) + "\n")
                    bw.close
                }
                Thread.sleep(300*1000)
                system.terminate()
            case _ =>
                println("Invalid job type!")
        }
    }
}