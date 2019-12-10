package mandelbrot

import scala.math._

/**
 * 複素数
 * http://www.stoyanr.com/2013/02/complex-numbers-in-scala.html
 * @param re
 * @param im
 */
case class Complex(re: Double, im: Double) extends Ordered[Complex] {
  private val modulus = sqrt(normSqr)

  val normSqr = pow(re, 2) + pow(im, 2)

  // Constructors
  def this(re: Double) = this(re, 0)

  // Unary operators
  def unary_+ = this
  def unary_- = new Complex(-re, -im)
  def unary_~ = new Complex(re, -im) // conjugate
  def unary_! = modulus

  // Comparison
  def compare(that: Complex) = !this compare !that

  // Arithmetic operations
  def +(c: Complex) = new Complex(re + c.re, im + c.im)
  def -(c: Complex) = this + -c
  def *(c: Complex) =
    new Complex(re * c.re - im * c.im, im * c.re + re * c.im)
  def /(c: Complex) = {
    require(c.re != 0 || c.im != 0)
    val d = pow(c.re, 2) + pow(c.im, 2)
    new Complex((re * c.re + im * c.im) / d, (im * c.re - re * c.im) / d)
  }

  // String representation
  override def toString() =
    this match {
      case Complex.i => "i"
      case Complex(re, 0) => re.toString
      case Complex(0, im) => im.toString + "*i"
      case _ => asString
    }
  private def asString =
    re + (if (im < 0) "-" + -im else "+" + im) + "*i"
}

object Complex {
  // Constants
  val i = Complex(0, 1)

  val zero = Complex(0,0)

  // Factory methods
  def apply(re: Double) = new Complex(re)

  // Implicit conversions
  implicit def fromDouble(d: Double) = new Complex(d)
  implicit def fromFloat(f: Float) = new Complex(f)
  implicit def fromLong(l: Long) = new Complex(l)
  implicit def fromInt(i: Int) = new Complex(i)
  implicit def fromShort(s: Short) = new Complex(s)
}


case class ComplexBD(re: BigDecimal, im: BigDecimal) extends Ordered[ComplexBD] {
  val normSqr = re.pow(2) + im.pow(2)

  // Constructors
  def this(re: BigDecimal) = this(re, 0)

  // Unary operators
  def unary_+ = this
  def unary_- = new ComplexBD(-re, -im)
  def unary_~ = new ComplexBD(re, -im) // conjugate

  // Comparison
  def compare(that: ComplexBD) = this.normSqr compare that.normSqr

  // Arithmetic operations
  def +(c: ComplexBD) = new ComplexBD(re + c.re, im + c.im)
  def -(c: ComplexBD) = this + -c
  def *(c: ComplexBD) =
    new ComplexBD(re * c.re - im * c.im, im * c.re + re * c.im)
  def /(c: ComplexBD) = {
    require(c.re != 0 || c.im != 0)
    val d = c.re.pow(2) + c.im.pow(2)
    new ComplexBD((re * c.re + im * c.im) / d, (im * c.re - re * c.im) / d)
  }

  // String representation
  override def toString() =
    this match {
      case ComplexBD.i => "i"
      case ComplexBD(re, x) if (x==0) => re.toString
      case ComplexBD(x, im) if (x==0) => im.toString + "*i"
      case _ => asString
    }
  private def asString =
    re + (if (im < 0) "-" + -im else "+" + im) + "*i"
}

object ComplexBD {
  // Constants
  val i = ComplexBD(0, 1)

  val zero = ComplexBD(0,0)

  // Factory methods
  def apply(re: BigDecimal) = new ComplexBD(re)

  // Implicit conversions
  implicit def fromDouble(d: Double) = new ComplexBD(d)
  implicit def fromFloat(f: Float) = new ComplexBD(f.toDouble)
  implicit def fromLong(l: Long) = new ComplexBD(l)
  implicit def fromInt(i: Int) = new ComplexBD(i)
  implicit def fromShort(s: Short) = new ComplexBD(s.toInt)
}
