package com.guidedog.directions

import com.google.maps.model.TravelMode
import com.google.maps.{DirectionsApi, GeoApiContext}

import scala.concurrent.{ExecutionContext, Future}

trait Directions {

  import Directions._

  val context = new GeoApiContext().setApiKey(apiKey)

  def directions(origin: String, destination: String)(implicit ec: ExecutionContext) = Future {
    val req = DirectionsApi.newRequest(context)
      .mode(TravelMode.WALKING)
      .units(com.google.maps.model.Unit.METRIC)
      .region("uk")
      .origin(origin)
      .destination(destination)
  }

}

object Directions {

  val apiKey = "AIzaSyCiWUfRtqpWK2Y7xLqY-E9y7qZUB1ZuCew"

}
