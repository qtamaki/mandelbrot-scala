package mandelbrot

object Main extends App {
  println("Hello")
  val m = Mandelbrot(50,10, Complex(-2,-2), Complex(2,2))
  Mandelbrot.printText(m)
  println()

  wandering(100, Wander(m, (0,0), (1.0/10)))

  def move(n: Int, m: Mandelbrot): Unit = {
    if (n == 0) return
    Mandelbrot.printText(m)
    println()
    val (x,y) = m.center
    move(n-1, m.moveCenter(x+1, y+1))
  }

  def zoom(n: Int, m: Mandelbrot): Unit = {
    if (n == 0) return
    Mandelbrot.printText(m)
    println()
    move(n-1, m.zoom(0.95))
  }

  def wandering(n: Int, w: Wander): Unit = {
    if (n == 0) return
    Mandelbrot.printText(w.m)
    println()
    println(s"m: ${w.m}, p: ${w.m.probe}")
    wandering(n-1, w.next)
  }
}

case class Wander(m: Mandelbrot, direction: (Int, Int), velocity: Double) {
  def turn:(Int, Int) = {
    val (x,y) = direction
    (x * -1, y * -1)
  }
  def detect: (Int, Int) = {
    val (x,y) = m.center
    val l = List(-1, 0 ,1)
    (for {
      i <- l
      j <- l
    } yield {
      (m.probe(x+i, y+j), (i, j))
    }).minBy{case (x, (a,b)) => (x+9)%10}._2
  }
  def next: Wander = {
      val d = detect
      if (detect == (0,0)) {
        println(s"zoom ${d}")
        Wander(m = m.zoom(1-velocity), d, velocity)
      }
      else {
        println(s"move ${d}")
        Wander(m = moveMandelbrot(d), d, velocity)
      }
//    (probe, p) match {
//      case (0, 0) => this.copy(m = moveMandelbrot)
//      case (0, _) => Wander(m.zoom(1-velocity), turn, velocity, p)
//      case (_, 0) => Wander(m.zoom(1-velocity), turn, velocity, p)
//      case (_, _) => this.copy(m = moveMandelbrot)
//    }
  }

  def moveMandelbrot(direction: (Int, Int)): Mandelbrot = {
    val (x,y) = m.center
    m.move(x+direction._1, y+direction._2)
  }
}