package com.guidedog.core

import akka.actor.{FSM, Props}
import akka.pattern._
import com.guidedog.PhoneNumber
import com.guidedog.directions._
import com.guidedog.model.Sms

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

object NavigationFSM extends Directions {

  def props(number : PhoneNumber) = Props(classOf[NavigationFSM], number)

  sealed trait Command

  case object Navigate extends Command

  case class InputAddress(address: String) extends Command

  case class SelectOption(selectedOption: Integer) extends Command

  case object NextDirection extends Command

  case class Locations(list: List[Location]) extends Command

  case class Routes(list: List[Route]) extends Command

  sealed trait Place

  case class Origin(address: String) extends Place

  case class Destination(address: String) extends Place


  sealed trait State

  case object WaitingForNavigate extends State

  case object OriginSelection extends State

  case object DestinationSelection extends State

  case object RouteSelection extends State

  case object Guide extends State


  final case class Navigation(origins: Option[List[Location]] = None,
                              originSelection: Option[Location] = None,
                              destinations: Option[List[Location]] = None,
                              destinationSelection: Option[Location] = None,
                              routes: Option[List[Route]] = None,
                              routeSelection: Option[Route] = None,
                              remainingSteps: Option[List[Step]] = None) {

    def setOrigins(origin: String) = lookupLocation(origin).map(locations => this.copy(origins = Some(locations)))

    def selectOrigin(selection: Int) = this.copy(originSelection = origins.flatMap(list => Try(list(selection)).toOption))

    def setDestinations(destination: String) = lookupLocation(destination).map(locations => this.copy(destinations = Some(locations)))

    def selectDestination(selection: Int) = this.copy(originSelection = destinations.flatMap(list => Try(list(selection)).toOption))

    def getRoutes() = {
      for {
        origin <- originSelection
        destination <- destinationSelection
      } yield {

        directions(origin.placeId, destination.placeId)
      }
    }

    def selectRoute(selection: Int) = this.copy(routeSelection = routes.flatMap(list => Try(list(selection)).toOption))

    def useStep = this.copy(remainingSteps = remainingSteps.map(rs => rs.tail))

  }

}

import com.guidedog.core.NavigationFSM._

class NavigationFSM(number : PhoneNumber) extends FSM[State, Navigation] {

  startWith(OriginSelection, new Navigation())

  onTransition {
    case OriginSelection -> DestinationSelection => {
      val sms = Sms(to = number, content = "Where to ?")
      Clockwork.sendSMS(sms)
    }
    case DestinationSelection -> RouteSelection =>
      nextStateData.getRoutes() match {
        case Some(future) => future.map(Routes(_)).pipeTo(self)
        case None => self ! Routes(List.empty)
      }
    case RouteSelection -> Guide => {
      val sms = Sms(to = number, content = "Route selected, send \"next\" to start receiving instructions")
      Clockwork.sendSMS(sms)
    }
  }


  when(WaitingForNavigate){
    case Event(Navigate, data) =>
      val sms = Sms(to = number, content = "Where from ?")
      Clockwork.sendSMS(sms)
      goto(OriginSelection) using data
  }

  when(OriginSelection) {

    case Event(InputAddress(address), data) =>
      data.setOrigins(address).map { newData =>
        Locations(newData.origins.getOrElse(List.empty))
      } pipeTo self
      stay using data

    case Event(Locations(list), data) =>
      if (list.isEmpty){
        val sms = Sms(to = number, content = "Could not find the location")
        Clockwork.sendSMS(sms)
        stay using data
      }
      else {
        val locations = list.zipWithIndex
        val locationsAsStrings = locations.map(x =>  s"${x._2}:${x._1.formattedAddress}")
        val sms = Sms(to = number, content = ("Pick one  " :: locationsAsStrings).mkString("\n"))
        Clockwork.sendSMS(sms)
        stay using data.copy(origins = Some(list))
      }

    case Event(SelectOption(option), data) =>
      val newState = data.selectOrigin(option)
      newState.originSelection match {
        case Some(origin) =>
          goto(DestinationSelection) using data.selectOrigin(option)
        case None =>
          val sms = Sms(to = number, content = "Wrong input, try again")
          Clockwork.sendSMS(sms)
          stay using data
      }

  }

  when(DestinationSelection) {

    case Event(InputAddress(address), data) =>
      data.setDestinations(address).map { newData =>
        Locations(newData.destinations.getOrElse(List.empty))
      } pipeTo self
      stay using data

    case Event(Locations(list), data) =>
      if (list.isEmpty){
        val sms = Sms(to = number, content = "Could not find the location")
        Clockwork.sendSMS(sms)
        stay using data
      }
      else {
        val locations = list.zipWithIndex
        val locationsAsStrings = locations.map(x =>  s"${x._2}:${x._1.formattedAddress}")
        val sms = Sms(to = number, content = ("Pick one  " :: locationsAsStrings).mkString("\n"))
        Clockwork.sendSMS(sms)
        stay using data.copy(destinations = Some(list))
      }

    case Event(SelectOption(option), data) =>
      val newState = data.selectDestination(option)
      newState.destinationSelection match {
        case Some(origin) =>
          goto(DestinationSelection) using data.selectOrigin(option)
          goto(RouteSelection) using data
        case None =>
          val sms = Sms(to = number, content = "Wrong input, try again")
          Clockwork.sendSMS(sms)
          stay using data
      }

  }

  when(RouteSelection) {

    case Event(Routes(list), data) =>
      if (list.isEmpty)
      //sms not found
      //reset potentially
        stay
      else {
        val locations = list.zipWithIndex
        def formatRoute(route : Route) = route match {
          case RouteFound(distance, duration, _) => s"$distance ($duration)"
          case RouteNotFound => "ERROR"
        }
        val locationsAsStrings = locations.map(x =>  s"${x._2}:${formatRoute(x._1)}")
        val sms = Sms(to = number, content = ("Pick one  " :: locationsAsStrings).mkString("\n"))
        Clockwork.sendSMS(sms)
        stay using data.copy(routes = Some(list))
      }

    case Event(SelectOption(option), data) =>
      goto(Guide) using data.selectRoute(option)
  }

  when(Guide) {
    case Event(NextDirection, data) =>
      data.remainingSteps.getOrElse(Nil) match {
        case step :: tail => {
          val content = s"${step.instruction}, for ${step.distance} (${step.duration}})"
          val sms = Sms(to = number, content = content )
          Clockwork.sendSMS(sms)
        }
        case Nil => {
          val sms = Sms(to = number, content = "No more instructions left, you should have arrived by now" )
          Clockwork.sendSMS(sms)
        }
      }

      val newData = data.useStep
      if (newData.remainingSteps.isEmpty) {
        val sms = Sms(to = number, content = "You have arrived, enjoy the rest of your day :)" )
        Clockwork.sendSMS(sms)
        stop()
      } else {
        stay using data
      }
  }

}
