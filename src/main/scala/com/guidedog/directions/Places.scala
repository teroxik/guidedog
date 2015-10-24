package com.guidedog.directions

import com.google.maps.model.PlacesSearchResult
import com.google.maps.{PlacesApi, GeoApiContext}

import scala.concurrent.Future

object Places {

  val apiKey = "AIzaSyCiWUfRtqpWK2Y7xLqY-E9y7qZUB1ZuCew"
  val context = new GeoApiContext().setApiKey(apiKey)

  def searchPlaces(description: String): Future[List[PlacesSearchResult]] = Future {
    val req = PlacesApi.textSearchQuery(context, description)
    req.await().results.toList
  }

}
