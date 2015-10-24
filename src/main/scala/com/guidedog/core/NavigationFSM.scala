package com.guidedog.core

import java.util.concurrent.Future

import akka.actor.FSM
import com.guidedog.directions.{Location, Step, Directions, Route}

import scala.util.Try

object NavigationFSM extends Directions {

  sealed trait Command

  case class InputAddress(address: String)

  case class SelectOption(selectedOption: Integer)

  case object NextDirection

  case object AtDestination

  sealed trait Place

  case class Origin(address: String) extends Place

  case class Destination(address: String) extends Place


  sealed trait State

  case object OriginSelection extends State

  case object DestinationSelection extends State

  case object RouteSelection extends State

  case object Navigation extends State


  final case class Navigation(origins: Option[List[Location]] = None,
                              originSelection: Option[Location] = None,
                              destinations: Option[List[Location]] = None,
                              destinationSelection: Option[Location] = None,
                              routes: Option[List[Route]] = None,
                              routeSelection: Option[Route] = None,
                              remainingSteps : List[Step] = None) {

    def setOrigins(origin: String) = lookupLocation(origin).map(locations => this.copy(origins = Some(locations)))

    def selectOrigin(selection: Int) = this.copy(originSelection = origins.flatMap(list => Try(list(selection)).toOption))

    def setDestinations(destination: String) = lookupLocation(destination).map(locations => this.copy(destinations = Some(locations)))

    def selectDestination(selection: Int) = this.copy(originSelection = destinations.flatMap(list => Try(list(selection)).toOption))

    def setRoutes() = {
      for {
      origin <- originSelection
      destination <- destinationSelection }
      yield directions(origin.placeId,destination.placeId).
    }

    def selectDestination(selection: Int) = this.copy(originSelection = destinations.flatMap(list => Try(list(selection)).toOption))


    def setRoutes(routes: List[Route]) = this.copy(routes = Some(routes))

    def selectRoute(selection: Int) = this.copy(routeSelection = Some(selection))

    def selectedRoute: Option[Route] = routeSelection.flatMap(selection => routes.map(dest => dest(selection)))

//    def getCurrentDirection() = {
//      selectedRoute.map {
//        route => {
//          if (leg < route.legs.length && route.legs(leg).steps.length < step) {
//            route.legs(leg).steps(step)
//          }
//          else {
//            None
//          }
//        }
//      }
//    }

    def useStep = this.copy(remainingSteps = remainingSteps.tail)

  }

}

import com.guidedog.core.NavigationFSM._

class NavigationFSM extends FSM[State, Navigation] {

  startWith(OriginSelection, new Navigation())

  when(OriginSelection) {
    case Event(InputAddress(address), data) =>
      stay using data.setOrigins(address)
    case Event(SelectOption(option), data) =>
      goto(DestinationSelection) using data.selectOrigin(option)
  }

  when(DestinationSelection) {
    case Event(InputAddress(address), data) =>
      stay using data.setOrigin(address)
    case Event(SelectOption(option), data) =>
      goto(RouteSelection) using data.selectDestination(option)
  }

  when(RouteSelection) {
    case Event(SelectOption(option), data) =>
      goto(Navigation) using data.selectRoute(option)
  }

  when(Navigation) {
    case Event(NextDirection, data) =>
      stay using data
    case Event(AtDestination, _) =>
      stop()
  }

}
