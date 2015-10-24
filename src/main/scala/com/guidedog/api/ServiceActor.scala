package com.guidedog.api

import akka.actor.Actor

import scala.concurrent.ExecutionContext

class ServiceActor extends Actor with Service {

  def actorRefFactory = context
  implicit val ec: ExecutionContext = actorRefFactory.dispatcher

  def receive = runRoute(route)

}
