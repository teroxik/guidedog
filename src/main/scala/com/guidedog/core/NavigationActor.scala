package com.guidedog.core

import akka.actor.{ActorRef, FSM}
import com.google.maps.model.DirectionsRoute
import scala.concurrent.duration._

object NavigationFSM {

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


  final case class Navigation(origin: Option[List[String]] = None,
                              originSelection: Option[Int] = None,
                              destination: Option[List[String]] = None,
                              destinationSelection: Option[Int] = None,
                              routes: Option[List[DirectionsRoute]] = None,
                              routeSelection: Option[Int] = None,
                              leg: Int = 0,
                              step: Int = 0) {

    def setOrigin(newOrigin: String) = this.copy(origin = Some(List(newOrigin)))

    def selectOrigin(selection: Int) = this.copy(originSelection = Some(selection))

    def selectedOrigin: Option[String] = originSelection.flatMap(selection => origin.map(orig => orig(selection)))

    def setDestination(newDestination: String) = this.copy(destination = Some(List(newDestination)))

    def selectDestination(selection: Int) = this.copy(destinationSelection = Some(selection))

    def selectedDestination: Option[String] = destinationSelection.flatMap(selection => destination.map(dest => dest(selection)))

    def setRoutes(routes: List[DirectionsRoute]) = this.copy(routes = Some(routes))

    def selectRoute(selection: Int) = this.copy(routeSelection = Some(selection))

    def selectedRoute: Option[DirectionsRoute] = routeSelection.flatMap(selection => routes.map(dest => dest(selection)))

    def getCurrentDirection() = {
      selectedRoute.map {
        route => {
          if (leg < route.legs.length && route.legs(leg).steps.length < step) {
            route.legs(leg).steps(step)
          }
          else {
            None
          }
        }
      }
    }

    def useStep = this.copy(step = step+1)

  }

}

import NavigationFSM._

class NavigationFSM extends FSM[State, Navigation] {

  startWith(OriginSelection, new Navigation())

  when(OriginSelection) {
    case Event(InputAddress(address), data) =>
      stay using data.setOrigin(address)
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
      stay using data.
    case Event(AtDestination, _) =>
      stop()
  }

}
