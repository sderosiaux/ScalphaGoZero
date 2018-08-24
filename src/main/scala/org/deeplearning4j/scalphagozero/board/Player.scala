package org.deeplearning4j.scalphagozero.board
import org.deeplearning4j.scalphagozero.board.PlayerColor.{ Black, White }

/**
  * Player of a game of Go (either black or white)
  */
trait Player[+C <: PlayerColor] {
  def color: PlayerColor
  def other: Player[PlayerColor]
}
case object BlackPlayer extends Player[Black.type] {
  override def color: Black.type = Black
  override def other: Player[White.type] = WhitePlayer
}
case object WhitePlayer extends Player[White.type] {
  override def color: White.type = White
  override def other: Player[Black.type] = BlackPlayer
}
