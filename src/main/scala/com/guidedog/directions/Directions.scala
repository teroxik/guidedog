package com.guidedog.directions

import com.google.maps.model.TravelMode
import com.google.maps.{DirectionsApi, GeoApiContext, GeocodingApi}

import scala.concurrent.{ExecutionContext, Future}

case class PlaceId(id : String) {
  def format = s"place_id:$id"

}
case class Location(placeId: PlaceId, formattedAddress: String)

trait Directions {

  import Directions._

  val context = new GeoApiContext().setApiKey(apiKey)

  def directions(origin: PlaceId, destination: PlaceId)(implicit ec: ExecutionContext) = Future {
    DirectionsApi.newRequest(context)
      .mode(TravelMode.WALKING)
      .units(com.google.maps.model.Unit.METRIC)
      .region("uk")
      .origin(origin.format)
      .destination(destination.format)
      .await().toList
      .map(_.legs.head.steps.head.htmlInstructions.replaceAll("""<(?!\/?a(?=>|\s.​ ))\/?.​?>""", ""))
  }

  def lookupLocation(location: String)(implicit ec: ExecutionContext): Future[List[Location]] = Future {
    GeocodingApi.geocode(context, location).
      await().toList.
      map(x => Location(PlaceId(x.placeId), x.formattedAddress))
  }



}

object Directions {

  val apiKey = "AIzaSyCiWUfRtqpWK2Y7xLqY-E9y7qZUB1ZuCew"

}
