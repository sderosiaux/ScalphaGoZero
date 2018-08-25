package org.deeplearning4j.scalphagozero.scoring

import java.util

import org.deeplearning4j.scalphagozero.board.Point

import scala.collection.mutable

/**
  * Can we find a better name ?
  */
trait GamePointType
case object BlackStone extends GamePointType
case object WhiteStone extends GamePointType
case object BlackTerritory extends GamePointType
case object WhiteTerritory extends GamePointType
case object Dame extends GamePointType

/**
  * Class to track territory on the Go board
  *
  * @param territoryMap map of Go points to status codes
  *
  * @author Max Pumperla
  */
class Territory(territoryMap: mutable.HashMap[Point, GamePointType]) {

  var numBlackTerritory = 0
  var numWhiteTerritory = 0
  var numBlackStones = 0
  var numWhiteStones = 0
  var numDame = 0
  private val damePoints: util.ArrayList[Point] = new util.ArrayList[Point]()

  for ((point, status) <- territoryMap) {
    status match {
      case BlackStone     => numBlackStones += 1
      case WhiteStone     => numWhiteStones += 1
      case BlackTerritory => numBlackTerritory += 1
      case WhiteTerritory => numWhiteTerritory += 1
      case Dame =>
        numDame += 1
        damePoints.add(point)
    }
  }
}
