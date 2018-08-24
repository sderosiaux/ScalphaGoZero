package org.deeplearning4j.scalphagozero.board
import org.deeplearning4j.scalphagozero.board.PlayerColor.{ Black, White }

/**
  * Player of a game of Go (either black or white)
  */
trait Player[+C <: PlayerColor] {
  def color: PlayerColor
  def other: Player[PlayerColor]
}
case object PlayerBlack extends Player[Black.type] {
  override def color: Black.type = Black
  override def other: Player[White.type] = PlayerWhite
}
case object PlayerWhite extends Player[White.type] {
  override def color: White.type = White
  override def other: Player[Black.type] = PlayerBlack
}
