package org.deeplearning4j.scalphagozero.scoring

import org.deeplearning4j.scalphagozero.board.PlayerColor.{ Black, White }
import org.deeplearning4j.scalphagozero.board.{ GoBoard, PlayerColor, Point }

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Compute the result of a game
  *
  * @param blackPoints points black scored
  * @param whitePoints points white scored
  * @param komi the komi that was agreed to at the beginning of the game
  *
  * @author Max Pumperla
  */
final case class GameResult(blackPoints: Int, whitePoints: Int, komi: Double) {

  val winner: PlayerColor = if (blackPoints > whitePoints + komi) PlayerColor.Black else PlayerColor.White

  val winningMargin: Double = Math.abs(blackPoints - (whitePoints + komi))

  override def toString: String = {
    val white = whitePoints + komi
    winner match {
      case Black => "B+ " + (blackPoints - white)
      case White => "W+ " + (white - blackPoints)
    }
  }
}

object GameResult {

  /**
    * Compute the game result from the current state.
    *
    * @param gameState GameState instance
    * @return GameResult object
    */
  def computeGameResult(goBoard: GoBoard): GameResult = {
    val territory = evaluateTerritory(goBoard)
    new GameResult(
      territory.numBlackTerritory + territory.numBlackStones,
      territory.numWhiteStones + territory.numWhiteStones,
      7.5
    )
  }

  /**
    * Evaluate / estimate the territory currently on
    * the Go board
    *
    * @param goBoard GoBoard instance
    * @return Territory object
    */
  def evaluateTerritory(goBoard: GoBoard): Territory = {
    val statusMap = new mutable.HashMap[Point, GamePointType]()
    for (row <- 1 to goBoard.row; col <- 1 to goBoard.col) {
      val point = Point(row, col)
      if (!statusMap.contains(point)) {
        goBoard.getColor(point) match {
          case Some(color) =>
            statusMap.put(point, if (color == PlayerColor.Black) BlackStone else WhiteStone)
          case None =>
            val (group, neighbors) = collectRegion(point, goBoard)
            val fillWith =
              if (neighbors.size == 1) {
                val neighborColor: PlayerColor = neighbors.head
                if (neighborColor == PlayerColor.Black) BlackTerritory else WhiteTerritory
              } else {
                Dame
              }

            for (position <- group) {
              statusMap.put(position, fillWith)
            }
        }
      }
    }
    new Territory(statusMap)
  }

  private def collectRegion(startingPoint: Point, board: GoBoard): (List[Point], Set[PlayerColor]) = {
    val initialColor = board.getColor(startingPoint)

    val visitedColors = mutable.Set[PlayerColor]()
    val visitedPoints = ListBuffer[Point](startingPoint)

    val toVisitPoints = mutable.Stack[Point](startingPoint)
    while (toVisitPoints.nonEmpty) {
      val toVisit = toVisitPoints.pop()
      val toVisitColor = board.getColor(toVisit)
      toVisitColor.foreach(visitedColors += _)

      if (toVisitColor == initialColor) {
        val nextVisits = toVisit.neighbors.filter(board.isOnGrid).diff(visitedPoints)
        toVisitPoints.pushAll(nextVisits)
        visitedPoints += toVisit
      }
    }

    (visitedPoints.toList, visitedColors.toSet)
  }

}
