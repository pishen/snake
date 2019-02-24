import com.googlecode.lanterna._
import com.googlecode.lanterna.input.KeyType
import com.googlecode.lanterna.terminal._
import scala.util.Random

object Main extends App {
  val terminal = new DefaultTerminalFactory().createTerminal()
  terminal.enterPrivateMode()
  terminal.setCursorVisible(false)
  val tg = terminal.newTextGraphics()

  tg.drawRectangle(
    new TerminalPosition(0, 0),
    new TerminalSize(50, 30),
    '\u2588'
  )
  tg.drawLine(20, 15, 25, 15, '\u2588')
  terminal.flush()

  case class Point(x: Int, y: Int) {
    def up = Point(x, y - 1)
    def right = Point(x + 1, y)
    def down = Point(x, y + 1)
    def left = Point(x - 1, y)
  }

  val points = (1 to 48).flatMap(x => (1 to 28).map(y => Point(x, y))).toSet
  def growApple(snake: Seq[Point]) = {
    val apple = Random.shuffle((points -- snake).toSeq).head
    tg.setCharacter(apple.x, apple.y, '\u2588')
    apple
  }

  def update(snake: Seq[Point], apple: Point): Int = {
    Thread.sleep(150)
    val command = Option(terminal.pollInput()).map(_.getKeyType)
    val next = command match {
      case Some(KeyType.ArrowUp) =>
        snake.head.up
      case Some(KeyType.ArrowRight) =>
        snake.head.right
      case Some(KeyType.ArrowDown) =>
        snake.head.down
      case Some(KeyType.ArrowLeft) =>
        snake.head.left
      case _ =>
        if (snake.head.y == snake(1).y - 1) {
          snake.head.up
        } else if (snake.head.x == snake(1).x + 1) {
          snake.head.right
        } else if (snake.head.y == snake(1).y + 1) {
          snake.head.down
        } else {
          snake.head.left
        }
    }

    if (next.x > 0 && next.x < 49 && next.y > 0 && next.y < 29 && !snake.contains(next)) {
      if (next == apple) {
        val newSnake = next +: snake
        val newApple = growApple(newSnake)
        update(newSnake, newApple)
      } else {
        tg.setCharacter(next.x, next.y, '\u2588')
        tg.setCharacter(snake.last.x, snake.last.y, ' ')
        update(next +: snake.init, apple)
      }
    } else {
      0
    }
  }

  val initialSnake = (20 to 25).map(x => Point(x, 15)).reverse
  val initialApple = growApple(initialSnake)
  update(initialSnake, initialApple)

  terminal.readInput()
  terminal.exitPrivateMode()
}
