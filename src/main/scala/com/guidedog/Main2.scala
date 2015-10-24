package com.guidedog

import com.guidedog.directions.Directions

object Main2 extends App with Directions {
  import scala.concurrent.ExecutionContext.Implicits.global

  val origin = "Hatters Hostel, Newton Street, Mancnhester"
  val destination = "Picadilly Station, Manchester"

  val pipo = for {
    x <- lookupLocation(origin)
    y <- lookupLocation(destination)
    z <- directions(x.head.placeId, y.head.placeId)
  } yield {
      z
    }

  pipo.foreach(_.foreach(println))

  Thread.sleep(5000)

}
