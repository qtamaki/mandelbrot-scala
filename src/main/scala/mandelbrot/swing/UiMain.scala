package mandelbrot.swing

import java.awt.EventQueue
import java.util.concurrent.Executors

import mandelbrot._

import scala.collection.mutable.Queue
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutorService, Future}
import scala.swing._

object UiMain extends SimpleSwingApplication {
  import event._
  import java.awt.{Dimension, Graphics2D, Graphics, Image, Rectangle}
  import java.awt.{Color => AWTColor}

  val ec: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(Executors.newWorkStealingPool)

  val width = 1024
  val height = 768
  val colorDepth = 255
  val bluishGray = new AWTColor(200, 255, 255)
  val bluishRed = new AWTColor(255, 0, 0)
  val data: Array[Int] = Array.fill(width*height)(0)
  val queue: Queue[Point] = new scala.collection.mutable.Queue[Point]

  val start = System.currentTimeMillis

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
    g.setColor(bluishRed)
    g.drawLine(width/2,height/2,width/2,height/2)
  }

  def top: MainFrame = new MainFrame {
    title = "Mandelbrot viewer"
    contents = mainPanel
  }

  def mainPanel: Panel = new Panel {
    def wandering(n: Int, w: Wander): Runnable = {
      new Runnable(){
        override def run(): Unit = {
          if(n==0) {
            println("Time： " + ((System.currentTimeMillis - start)/1000) + " sec")
            return
          }
          val w_ = if(!queue.isEmpty) {
            val p = queue.dequeue()
            w.copy(m = w.m.move(p.x, p.y))
          } else w
          calc_p2(w_.m)
          repaint()
//          EventQueue.invokeLater(wandering(n-1, w_.next))
            EventQueue.invokeLater(wandering(n-1, w_.next_zoom))
        }
      }
    }

    val m = Mandelbrot(width, height, Complex(-2,-2), Complex(2,2))
    EventQueue.invokeLater(wandering(1000, Wander(m, (0,0), 1.0/30)))

    preferredSize = new Dimension(width, height)
    focusable = true
    listenTo(mouse.clicks)
    reactions += {
      case MouseClicked(_, point, _, _, _) =>
        println(s"mouse: $point")
        queue += point
    }
    override def paint(g: Graphics2D) {
      g setColor bluishGray
      g fillRect (0, 0, size.width, size.height)
      onPaint(g)
    }
  }

  def calc(m: Mandelbrot) = {
    val r = for {
      y <- 0 until height
      x <- 0 until width
    } {
      data(y*width+x) = m.probe(x,y,colorDepth)
    }
  }

  def calc_p(m: Mandelbrot) = {
    val r = for {
      y <- 0 until height
      x <- 0 until width
    } yield {
      Future({
        data(y*width+x) = m.probe(x,y,colorDepth)
      })(ec)
    }
    r.foreach(Await.ready(_, Duration.Inf))
  }

  def calc_p2(m: Mandelbrot) = {
    val r = for {
      y <- 0 until height
    } yield {
      Future({
        for {
          x <- 0 until width
        } {
          data(y * width + x) = m.probe(x, y, colorDepth)
        }
      })(ec)
    }
    r.foreach(Await.ready(_, Duration.Inf))
  }
}
