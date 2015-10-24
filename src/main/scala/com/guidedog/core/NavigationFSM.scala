package com.guidedog.core


import akka.actor.FSM
import akka.pattern._
import com.guidedog.directions.{Location, Step, Directions, Route}

import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global

object NavigationFSM extends Directions {

  sealed trait Command

  case class InputAddress(address: String)

  case class SelectOption(selectedOption: Integer)

  case object NextDirection

  case class Locations(list: List[Location])

  case class Routes(list: List[Route])

  sealed trait Place

  case class Origin(address: String) extends Place

  case class Destination(address: String) extends Place


  sealed trait State

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

class NavigationFSM extends FSM[State, Navigation] {

  startWith(OriginSelection, new Navigation())

  onTransition {
    case OriginSelection -> DestinationSelection => //send destination selection
    case DestinationSelection -> RouteSelection =>
      nextStateData.getRoutes() match {
        case Some(future) => future.map(Routes(_)).pipeTo(self)
        case None => self ! Routes(List.empty)
      }
    case RouteSelection -> Guide => //send route selection
  }



  when(OriginSelection) {

    case Event(InputAddress(address), data) =>
      data.setOrigins(address).map { newData =>
        Locations(newData.origins.getOrElse(List.empty))
      } pipeTo self
      stay using data

    case Event(Locations(list), data) =>
      if (list.isEmpty)
      //sms not found
        stay using data
      else {
        //sms found
        stay using data.copy(origins = Some(list))
      }

    case Event(SelectOption(option), data) =>
      val newState = data.selectOrigin(option)
      newState.originSelection match {
        case Some(origin) =>
          goto(DestinationSelection) using data.selectOrigin(option)
        case None =>
          //sms reply not found
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
      if (list.isEmpty)
      //sms not found
        stay using data
      else {
        //sms found
        stay using data.copy(destinations = Some(list))
      }

    case Event(SelectOption(option), data) =>
      val newState = data.selectDestination(option)
      newState.destinationSelection match {
        case Some(origin) =>
          goto(RouteSelection) using data
        case None =>
          //sms reply not found
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
        //sms found
        stay using data.copy(routes = Some(list))
      }

    case Event(SelectOption(option), data) =>
      goto(Guide) using data.selectRoute(option)
  }

  when(Guide) {
    case Event(NextDirection, data) =>
      //reply step
      val newData = data.useStep
      if (newData.remainingSteps.isEmpty) {
        //finish message
        stop()
      } else {
        stay using data
      }
  }

}
