package com.guidedog.api

import com.guidedog.core.Clockwork
import com.guidedog.model.Sms
import spray.routing.HttpService

import scala.concurrent.ExecutionContext

trait Service extends HttpService {

  implicit val ec: ExecutionContext

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
            val sms = Sms(Some(from), to, content, msg_id, keyword)
            println(sms)
            "SMS Received"
          }
        }
      }
    }

}
