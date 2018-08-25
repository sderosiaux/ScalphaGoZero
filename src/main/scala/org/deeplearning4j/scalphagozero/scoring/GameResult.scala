package org.deeplearning4j.scalphagozero.scoring

import org.deeplearning4j.scalphagozero.board.{ GoBoard, Point, _ }

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

  val winner: Player = if (blackPoints > whitePoints + komi) BlackPlayer else WhitePlayer

  val winningMargin: Double = Math.abs(blackPoints - (whitePoints + komi))

  override def toString: String = {
    val white = whitePoints + komi
    winner match {
      case BlackPlayer => "B+ " + (blackPoints - white)
      case WhitePlayer => "W+ " + (white - blackPoints)
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
        goBoard.getPlayer(point) match {
          case Some(color) =>
            statusMap.put(point, if (color == BlackPlayer) BlackStone else WhiteStone)
          case None =>
            val (group, neighbors) = collectRegion(point, goBoard)
            val fillWith =
              if (neighbors.size == 1) {
                val neighborColor: Option[Player] = neighbors.head
                if (neighborColor.get == BlackPlayer) BlackTerritory else WhiteTerritory
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

  private[this] final val deltas = List((-1, 0), (1, 0), (0, -1), (0, 1))

  // Is it possible to rewrite this in a tailrec function ?
  // And/or use another mecanism to browse the board ?
  // Why not use the `Point(...).neighbors` method ?
  private def collectRegion(
      startingPoint: Point,
      board: GoBoard,
      visited: Set[Point] = Set.empty
  ): (List[Point], Set[Option[Player]]) =
    if (visited.contains(startingPoint)) (List.empty, Set.empty)
    else {
      val here: Option[Player] = board.getPlayer(startingPoint)

      val (allPoints, allBorders) =
        deltas.foldLeft((ListBuffer(startingPoint), mutable.Set.empty[Option[Player]])) {
          case ((pointsAcc, bordersAcc), (row, col)) =>
            val nextPoint = Point(startingPoint.row + row, startingPoint.col + col)
            if (board.isOnGrid(nextPoint)) {
              val neighbor: Option[Player] = board.getPlayer(nextPoint)
              if (neighbor == here) {
                val (points, borders) = collectRegion(nextPoint, board, visited + startingPoint)
                (pointsAcc ++ points, bordersAcc ++ borders)
              } else {
                (pointsAcc, bordersAcc + neighbor)
              }
            } else (pointsAcc, bordersAcc)
        }

      (allPoints.toList, allBorders.toSet)
    }

}
