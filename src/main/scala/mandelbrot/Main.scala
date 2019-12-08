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

