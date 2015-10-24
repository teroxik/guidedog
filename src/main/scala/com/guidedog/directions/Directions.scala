package com.guidedog.directions

import com.google.maps.model.TravelMode
import com.google.maps.{DirectionsApi, GeoApiContext, GeocodingApi}
import org.jsoup.Jsoup

import scala.concurrent.{ExecutionContext, Future}

case class PlaceId(id: String) {
  def format = s"place_id:$id"

}

case class Location(placeId: PlaceId, formattedAddress: String)
case class Step(distance : String, duration : String, instruction : String)

trait Route
case class RouteFound(distance: String, duration: String, instructions: List[Step]) extends Route
case object RouteNotFound extends Route

trait Directions {

  import Directions._

  val context = new GeoApiContext().setApiKey(apiKey)

  def directions(origin: PlaceId, destination: PlaceId)(implicit ec: ExecutionContext): Future[List[Route]]
  = Future {
    DirectionsApi.newRequest(context)
      .mode(TravelMode.WALKING)
      .units(com.google.maps.model.Unit.METRIC)
      .region("uk")
      .origin(origin.format)
      .destination(destination.format)
      .await().toList
      .map(_.legs.headOption.map { //Just considering 1 leg for now (no waypoints)
        x => RouteFound(
          x.distance.humanReadable, 
          x.duration.humanReadable, 
          x.steps.map(s => Step(
            s.distance.humanReadable,
            s.duration.humanReadable,
            removeTags(s.htmlInstructions))).toList
        )
      }.getOrElse(RouteNotFound))
  }

  def lookupLocation(location: String)(implicit ec: ExecutionContext): Future[List[Location]] = Future {
    GeocodingApi.geocode(context, location).
      await().toList.
      map(x => Location(PlaceId(x.placeId), x.formattedAddress))
  }


}

object Directions {
  /**
   * Removes HTML tags from a string
   */
  def removeTags(html : String) = Jsoup.parse(html).text()

  val apiKey = "AIzaSyCiWUfRtqpWK2Y7xLqY-E9y7qZUB1ZuCew"

}
