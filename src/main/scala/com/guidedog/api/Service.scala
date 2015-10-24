package com.guidedog.api

import akka.actor.ActorRef
import com.guidedog.core.Clockwork
import com.guidedog.core.NavigationFSM.{AtDestination, NextDirection, InputAddress, SelectOption}
import com.guidedog.model.Sms
import spray.routing.HttpService

import scala.concurrent.ExecutionContext
import scala.util.Try
import scalaz._
import Scalaz._

trait Service extends HttpService {

  implicit val ec: ExecutionContext

  val navigation: ActorRef

  val route = path ("") {
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
            val command = content.trim.toLowerCase
            if (content.trim.toLowerCase == "navigate") {
              ???
            } else if (content.trim.toLowerCase == "next") {
              navigation ! NextDirection
            } else if (content.trim.toLowerCase == "finish") {
              navigation ! AtDestination
            } else {
              Try(content.trim.toInt).toOption.cata(navigation ! SelectOption(_), navigation ! InputAddress(command))
            }
            "SMS Received"
          }
        }
      }
    }

}
