import com.googlecode.lanterna._
import com.googlecode.lanterna.input.KeyType
import com.googlecode.lanterna.terminal._

object Main extends App {
  val terminal = new DefaultTerminalFactory().createTerminal()
  terminal.enterPrivateMode()
  terminal.setCursorVisible(false)
  val tg = terminal.newTextGraphics()

  // draw the edges
  tg.drawRectangle(
    new TerminalPosition(0, 0),
    new TerminalSize(50, 30),
    '\u2588'
  )
  tg.drawLine(20, 15, 25, 15, '\u2588')
  terminal.flush()

  def update(snake: Seq[(Int, Int)]): Int = {
    Thread.sleep(300)
    val command = Option(terminal.pollInput()).map(_.getKeyType)
    val next = command match {
      case Some(KeyType.ArrowUp) =>
        (snake.head._1, snake.head._2 - 1)
      case Some(KeyType.ArrowRight) =>
        (snake.head._1 + 1, snake.head._2)
      case Some(KeyType.ArrowDown) =>
        (snake.head._1, snake.head._2 + 1)
      case Some(KeyType.ArrowLeft) =>
        (snake.head._1 - 1, snake.head._2)
      case _ =>
        if (snake.head._1 == snake(1)._1 + 1) {
          (snake.head._1 + 1, snake.head._2)
        } else if (snake.head._2 == snake(1)._2 + 1) {
          (snake.head._1, snake.head._2 + 1)
        } else if (snake.head._1 == snake(1)._1 - 1) {
          (snake.head._1 - 1, snake.head._2)
        } else {
          (snake.head._1, snake.head._2 - 1)
        }
    }

    if (next._1 > 0 && next._1 < 49 && next._2 > 0 && next._2 < 29) {
      tg.setCharacter(next._1, next._2, '\u2588')
      tg.setCharacter(snake.last._1, snake.last._2, ' ')
      update(next +: snake.init)
    } else {
      0
    }
  }
  update((20 to 25).map(_ -> 15).reverse)
  terminal.readInput()
  terminal.exitPrivateMode()
}
