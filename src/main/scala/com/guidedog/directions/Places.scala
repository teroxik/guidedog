package com.guidedog.directions

import com.google.maps.model.{LatLng, PlacesSearchResult}
import com.google.maps.{PlacesApi, GeoApiContext}

import scala.concurrent.{ExecutionContext, Future}

object Places {

  val apiKey = "AIzaSyCiWUfRtqpWK2Y7xLqY-E9y7qZUB1ZuCew"
  val context = new GeoApiContext().setApiKey(apiKey)

  def searchPlaces(description: String, near: LatLng)(implicit ec : ExecutionContext): Future[List[Location]] = Future {
    val req = PlacesApi.textSearchQuery(context, description)
      .location(near)
      .radius(2000) // roughly one mile radius
    req.await().results.toList.map(r => Location(PlaceId(r.placeId), r.formattedAddress, r.geometry.location))
  }

  def searchPlaces(description: String)(implicit ec: ExecutionContext): Future[List[Location]] = Future {
    val req = PlacesApi.textSearchQuery(context, "uk " + description)
    req.await().results.toList.map(r => Location(PlaceId(r.placeId), r.formattedAddress, r.geometry.location))
  }

  def extractPlaces(xs: List[PlacesSearchResult]): List[String] =
    xs.map(r => r.name)

}
