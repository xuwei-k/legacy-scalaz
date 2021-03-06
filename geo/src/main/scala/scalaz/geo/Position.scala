package scalaz
package geo

sealed trait Position {
  val coord: Coord
  val elevation: Elevation
}

trait Positions {
  def position(c: Coord, e: Elevation) = new Position {
    val coord = c
    val elevation = e
  }
}

object Position {
  import Predef.{implicitly => i}
  import Scalaz._

  implicit def PositionShow: Show[Position] = i[Show[(Coord, Elevation)]] ∙ (((_: Position).coord) &&& ((_: Position).elevation))

  implicit def PositionEqual: Equal[Position] = i[Equal[(Coord, Elevation)]] ∙ (((_: Position).coord) &&& ((_: Position).elevation))

  implicit def PositionOrder: Order[Position] = i[Order[(Coord, Elevation)]] ∙ (((_: Position).coord) &&& ((_: Position).elevation))  
}