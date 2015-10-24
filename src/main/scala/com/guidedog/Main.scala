package com.guidedog

import akka.actor.{Props, ActorSystem}
import akka.io.IO
import akka.util.Timeout
import akka.pattern.ask
import com.guidedog.api.ServiceActor
import scala.concurrent.duration._
import spray.can.Http

object Main extends App {

  implicit val timeout = Timeout(5.seconds)
  implicit val system = ActorSystem("guidedog")

  val service = system.actorOf(Props[ServiceActor], "guidedog-service")

  IO(Http) ? Http.Bind(service, interface = "localhost", port = 8080)

}
