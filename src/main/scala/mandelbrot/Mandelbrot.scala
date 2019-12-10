package mandelbrot

import scala.annotation.tailrec

case class Mandelbrot(width: Int, height: Int, lowerComplex: Complex, distanceRe: Double, distanceIm: Double) {
  val center:(Int, Int) = (width/2, height/2)

  def pointToComp(x: Int, y: Int): Complex = Mandelbrot.pointToComp(this, x, y)

  def move(x: Int, y: Int): Mandelbrot = {
    val (cx, cy) = center
    val re_ = lowerComplex.re + distanceRe * (1.0 * (x - cx) / width)
    val im_ = lowerComplex.im + distanceIm * (1.0 * (y - cy) / height)
    copy(lowerComplex = Complex(re_, im_))
  }

  def moveCenter(x: Int, y: Int): Mandelbrot = {
    val (cx, cy) = center
    // センターとの距離x2分、範囲が小さくなる
    val reScale = 1.0 * (width - Math.abs(cx - x) * 2) / width
    val imScale = 1.0 * (height - Math.abs(cy - y) * 2) / height
    // より小さくなる方に合わせてスケールする
    val scale = Math.min(reScale, imScale)
    // 中心とスケールを揃えてズームする
    resize(x, y, scale)
  }

  def resize(x: Int, y: Int, scale: Double): Mandelbrot = {
    // サイズから始点を割り出す
    val lowX = x - (width * scale / 2).toInt
    val lowY = y - (height * scale / 2).toInt
    Mandelbrot(width, height, pointToComp(lowX, lowY), distanceRe * scale, distanceIm * scale)
  }

  def zoom(scale: Double): Mandelbrot = {
    val (x,y) = center
    resize(x,y, scale)
  }

  /**
   * 現在の中心の状態を調べる
   * @param m
   * @return 0 - 9 で残回数が返る
   */
  def probe: Int = {
    val (x,y) = center
    probe(x,y)
  }
  def probe(x: Int, y: Int): Int = {
    probe(x,y,9)
  }
  def probe(x: Int, y: Int, limit: Int): Int = {
    val c = pointToComp(x, y)
    Mandelbrot.divergeTime(Complex.zero, c, limit)
  }
}

object Mandelbrot {
  def apply(width: Int, height: Int, lowerComplex: Complex, upperComplex: Complex): Mandelbrot = {
    Mandelbrot(width, height, lowerComplex, upperComplex.re - lowerComplex.re, upperComplex.im - lowerComplex.im)
  }

  @tailrec
  def divergeTime(z: Complex, c: Complex, limit: Int): Int = {
    val z_ = z * z + c
    if (z_.normSqr > 4.0 || limit == 0) limit else divergeTime(z_, c, limit - 1)
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
  def printText(m: Mandelbrot): Unit = {
    val z = Complex.zero
    for {
      y <- 0 until m.height
      x <- 0 until m.width
    } {
      val r = m.probe(x,y)
      if(x == m.width-1) println(r) else print (r)
    }
  }
}