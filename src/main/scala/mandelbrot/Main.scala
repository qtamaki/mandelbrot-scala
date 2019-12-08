package mandelbrot

object Main extends App {
  println("Hello")
  val m = Mandelbrot(200,50, Complex(-2,-2), Complex(2,2))
  Mandelbrot.printText(m)
}
