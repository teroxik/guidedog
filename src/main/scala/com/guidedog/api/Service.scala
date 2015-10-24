package com.guidedog.api

import akka.actor.ActorRef
import com.guidedog.PhoneNumber
import com.guidedog.core.Clockwork
import com.guidedog.core.NavigationFSM.{AtDestination, InputAddress, NextDirection, SelectOption}
import com.guidedog.model.Sms
import spray.routing.HttpService

import scala.concurrent.ExecutionContext
import scala.util.Try
import scalaz.Scalaz._

trait Service extends HttpService {

  implicit val ec: ExecutionContext

  def createNavigator(number: PhoneNumber): ActorRef

  val navigators: akka.agent.Agent[Map[PhoneNumber, ActorRef]]

  val route = path("") {
    get {
      parameter("to") { (to) =>
        complete {
          val sms = Sms(None, to, "Hello World", None, None)
          Clockwork.sendSMS(sms).map(_.toString)
        }
      }
    }
  } ~
    pathPrefix("sms") {
      (get | post) {
        parameter("from", "to", "content", "msg_id".?, "keyword".?) { (from, to, content, msg_id, keyword) =>
          complete {
            val sms = Sms(Some(from), to, content, msg_id, keyword)
            val navigator = {
              val maybeNav = navigators.get.get(from)
              maybeNav.getOrElse {
                val nav = createNavigator(from)
                navigators.alter(map => map + (from -> nav))
                nav
              }
            }
            val command = content.trim.toLowerCase
            if (command == "navigate") {
              ???
            } else if (command == "next") {
              navigator ! NextDirection
            } else if (command == "finish") {
              navigator ! AtDestination
            } else {
              Try(content.trim.toInt).toOption.cata(navigator ! SelectOption(_), navigator ! InputAddress(command))
            }
            "SMS Received"
          }
        }
      }
    }

}
