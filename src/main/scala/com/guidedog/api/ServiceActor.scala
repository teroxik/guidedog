package com.guidedog.api

import akka.actor.{Props, ActorRef, Actor}
import akka.agent.Agent
import com.guidedog.PhoneNumber
import com.guidedog.core.NavigationFSM

import scala.concurrent.ExecutionContext

class ServiceActor(val navigators : Agent[Map[PhoneNumber, ActorRef]]) extends Actor with Service {

  def actorRefFactory = context
  implicit val ec: ExecutionContext = actorRefFactory.dispatcher

  def receive = runRoute(route)

  override def createNavigator(number: PhoneNumber): ActorRef = context.actorOf(NavigationFSM.props(number))

}

object ServiceActor{

  def props(navigators : Agent[Map[PhoneNumber, ActorRef]]) = Props(new ServiceActor(navigators))

}
