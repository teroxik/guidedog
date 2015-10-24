package com.guidedog

import akka.actor.{ActorRef, Props, ActorSystem}
import akka.agent.Agent
import akka.io.IO
import akka.util.Timeout
import akka.pattern.ask
import com.guidedog.api.ServiceActor
import scala.concurrent.duration._
import spray.can.Http

object Main extends App {

  implicit val timeout = Timeout(5.seconds)
  implicit val system = ActorSystem("guidedog")
  val navigators = Agent(Map.apply[PhoneNumber, ActorRef]())
  val service = system.actorOf(ServiceActor.props(navigators), "guidedog-service")

  IO(Http) ? Http.Bind(service, interface = "0.0.0.0", port = 8080)

}
