package com.guidedog

import com.guidedog.directions.Directions

import scala.concurrent.Await
import scala.concurrent.duration._

object Main2 extends App with Directions {
  import scala.concurrent.ExecutionContext.Implicits.global

  val origin = "Hatters Hostel, Newton Street, Mancnhester"
  val destination = "Picadilly Station, Manchester"

  val pipo = directions(origin, destination)

  val result = Await.result(pipo, 5 seconds)

}
