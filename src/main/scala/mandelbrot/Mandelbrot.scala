package mandelbrot

import scala.annotation.tailrec

case class Mandelbrot(width: Int, height: Int, lowerComplex: Complex, distanceRe: Double, distanceIm: Double) {
  def pointToComp(x: Int, y: Int): Complex = Mandelbrot.pointToComp(this, x, y)
}

object Mandelbrot {
  def apply(width: Int, height: Int, lowerComplex: Complex, upperComplex: Complex): Mandelbrot = {
    Mandelbrot(width, height, lowerComplex, upperComplex.re - lowerComplex.re, upperComplex.im - lowerComplex.im)
  }

  @tailrec
  def divergeTime(z: Complex, c: Complex, limit: Int): Int = {
    val z_ = z * z + c
    if (!z_ > 4.0 || limit == 0) limit else divergeTime(z_, c, limit - 1)
  }

  def pointToComp(m: Mandelbrot, x: Int, y: Int): Complex = {
    val xf = m.lowerComplex.re + x * m.distanceRe / m.width
    val yf = m.lowerComplex.im + y * m.distanceIm / m.height
    Complex(xf,yf)
  }

  /**
   * テキストで表示させる
   * @param m
   */
  def printText(m: Mandelbrot) {
    val z = Complex.zero
    for {
      y <- 0 until m.height
      x <- 0 until m.width
    } {
      val c = pointToComp(m, x, y)
      val r = divergeTime(z, c, 9)
      if(x == m.width-1) println(r) else print (r)
    }
  }
}