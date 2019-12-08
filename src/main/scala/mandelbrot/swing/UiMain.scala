package mandelbrot.swing

import mandelbrot._

import scala.swing._

object UiMain extends SimpleSwingApplication {
  import event._
  import event.Key._
  import java.awt.{Dimension, Graphics2D, Graphics, Image, Rectangle}
  import java.awt.{Color => AWTColor}

  val width = 1024
  val height = 768
  val colorDepth = 255
  val bluishGray = new AWTColor(200, 255, 255)
  val bluishSilver = new AWTColor(210, 255, 255)
  val data: Array[Int] = Array.fill(width*height)(0)

  calc

  def onKeyPress(keyCode: Value) = keyCode match {
    case _ => // do something
  }
  def onPaint(g: Graphics2D) {
    for {
      y <- 0 until height
      x <- 0 until width
    } {
      val c = data(y*width+x)
      val b = new AWTColor(c,c,c)
      g.setColor(b)
      g.drawLine(x,y,x,y)
    }
  }

  def top: MainFrame = new MainFrame {
    title = "Mandelbrot viewer"
    contents = mainPanel
  }

  def mainPanel: Panel = new Panel {
    preferredSize = new Dimension(width, height)
    focusable = true
    listenTo(keys)
    reactions += {
      case KeyPressed(_, key, _, _) =>
        onKeyPress(key)
        repaint
    }
    override def paint(g: Graphics2D) {
      g setColor bluishGray
      g fillRect (0, 0, size.width, size.height)
      onPaint(g)
    }
  }

  def calc = {
    val m = Mandelbrot(width, height, Complex(-2,-2), Complex(2,2))
    for {
      y <- 0 until height
      x <- 0 until width
    } {
      data(y*width+x) = m.probe(x,y,colorDepth)
    }
  }
}
