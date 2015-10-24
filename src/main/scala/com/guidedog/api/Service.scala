package com.guidedog.api

import spray.routing.{Route, HttpService}

import scala.concurrent.ExecutionContext

trait Service extends HttpService {

  implicit val ec: ExecutionContext

  val route = path ("") {
    get {
      complete {
        "Hello World"
      }
    }
  }

}
