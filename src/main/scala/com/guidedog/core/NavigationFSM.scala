package com.guidedog.core

import akka.actor.{LoggingFSM, Props}
import akka.pattern._
import com.google.maps.model.LatLng
import com.guidedog.PhoneNumber
import com.guidedog.directions._
import com.guidedog.model.Sms

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

object NavigationFSM extends Directions {

  def props(number: PhoneNumber) = Props(new NavigationFSM(number))

  //
  // COMMANDS
  //
  sealed trait Command
  case object Navigate extends Command
  case class InputAddress(address: String) extends Command
  case class SelectOption(selectedOption: Integer) extends Command
  case object NextDirection extends Command
  case class Locations(list: List[Location]) extends Command
  case class Routes(list: List[Route]) extends Command


  //
  // the fuck is that ?
  //
  sealed trait Place
  case class Origin(address: String) extends Place
  case class Destination(address: String) extends Place

  //
  // STATES
  //
  sealed trait State
  case object WaitingForNavigate extends State
  case object OriginSelection extends State
  case object DestinationSelection extends State
  case object Guide extends State


  //
  // FSM DATA
  //
  final case class Navigation(origins: Option[List[Location]] = None,
                              originSelection: Option[Location] = None,
                              destinations: Option[List[Location]] = None,
                              destinationSelection: Option[Location] = None,
                              remainingSteps: Option[List[Step]] = None) {

    def setOrigins(origin: String) = lookupLocation(origin).map(locations => this.copy(origins = Some(locations)))

    def selectOrigin(selection: Int) = this.copy(originSelection = origins.flatMap(list => Try(list(selection)).toOption))

    def setDestinations(destination: String, latlng: Option[LatLng]) = lookupLocation(destination, latlng).map(locations => this.copy(destinations = Some(locations)))

    def selectDestination(selection: Int) = this.copy(destinationSelection = destinations.flatMap(list => Try(list(selection)).toOption))

    def fetchRoutes() = {
      for {
        origin <- originSelection
        destination <- destinationSelection
      } yield {
        directions(origin.placeId, destination.placeId)
      }
    }

    def useStep = this.copy(remainingSteps = remainingSteps.map(rs => rs.tail))

  }

}

import com.guidedog.core.NavigationFSM._

class NavigationFSM(number: PhoneNumber) extends LoggingFSM[State, Navigation] {

  startWith(WaitingForNavigate, new Navigation())

  onTransition {
    case OriginSelection -> DestinationSelection => {
      val sms = Sms(to = number, content = "Where to ?")
      Clockwork.sendSMS(sms)
    }
    case DestinationSelection -> Guide => {
      nextStateData.fetchRoutes() match {
        case Some(future) =>
          future.map(x =>
            Routes(x)).pipeTo(self)
        case None => self ! Routes(List.empty)
      }
    }
  }


  when(WaitingForNavigate) {
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
      if (list.isEmpty) {
        val sms = Sms(to = number, content = "Could not find the location")
        Clockwork.sendSMS(sms)
        stay using data
      }
      else {
        val locations = list.zipWithIndex
        val locationsAsStrings = locations.map(x => s"${x._2}:${x._1.formattedAddress}")
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
      data.setDestinations(address, data.originSelection.map(_.longlat)).map { newData =>
        Locations(newData.destinations.getOrElse(List.empty))
      } pipeTo self
      stay using data

    case Event(Locations(list), data) =>
      if (list.isEmpty) {
        val sms = Sms(to = number, content = "Could not find the location")
        Clockwork.sendSMS(sms)
        stay using data
      }
      else {
        val locations = list.zipWithIndex.map{case (a,b) => (a, b +1)}
        val locationsAsStrings = locations.map(x => s"${x._2}:${x._1.formattedAddress}")
        val sms = Sms(to = number, content = ("Pick one  " :: locationsAsStrings).mkString("\n"))
        Clockwork.sendSMS(sms)
        stay using data.copy(destinations = Some(list))
      }

    case Event(SelectOption(option), data) =>
      val newState = data.selectDestination(option)
      newState.destinationSelection match {
        case Some(origin) =>
          goto(Guide) using newState
        case None =>
          val sms = Sms(to = number, content = "Wrong input, try again")
          Clockwork.sendSMS(sms)
          stay using data
      }

  }


  when(Guide) {
    case Event(Routes(list), data) =>
      if (list.isEmpty) {
        val sms = Sms(to = number, content = "Couldn't find a route, sorry")
        Clockwork.sendSMS(sms)
        stay using data
      }
      else {
        list.head match {
          case r@RouteFound(distance, duration, steps) =>
            val sms = Sms(to = number, content = "Route selected, send \"next\" to start receiving instructions")
            Clockwork.sendSMS(sms)
            goto(Guide) using data.copy(remainingSteps = Some(steps))
        }
      }

    case Event(NextDirection, data) =>
      data.remainingSteps.getOrElse(Nil) match {
        case step :: tail => {
          val content = s"${step.instruction}, for ${step.distance} (${step.duration})"
          val sms = Sms(to = number, content = content)
          Clockwork.sendSMS(sms)
          stay() using data.useStep
        }
        case Nil => {
          val sms = Sms(to = number, content = "You have arrived at your destination, thanks for using GuideDog, have a nice day.")
          Clockwork.sendSMS(sms)
          stay()
        }
      }
  }

  whenUnhandled {
    case Event(Navigate, _) =>
      val sms = Sms(to = number, content = "Where from ?")
      Clockwork.sendSMS(sms)
      goto(OriginSelection) using new Navigation()
  }

}
