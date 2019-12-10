package mandelbrot

import scala.annotation.tailrec

case class Wander(m: Mandelbrot, direction: (Int, Int), velocity: Double) {
  val moveX = (m.width*velocity).toInt
  val moveY = (m.height*velocity).toInt

  // 1方向に探索していく
  def search(direction: (Int,Int), threshold: Double): Int = {
    val (x,y) = m.center
    val (dx,dy) = direction
    val len = Math.min(x,y) // 最大探索範囲
    @tailrec
    def lineSearch(i: Int, sum: Int): Int = {
      if (i == len) return i
      else {
        val p = m.probe(x + dx * i, y + dy * i)
        print(p)
        val av = (sum + p) / (i + 1) // 平均を出す
        val n = Math.pow(p - av, 2)
        if (n > threshold) return i
        else lineSearch(i+1, sum + p)
      }
    }
    println()
    print("linesearch: ")
    lineSearch(0, 0)
  }

  def search2(direction: (Int,Int), threshold: Double): Int = {
    val (x,y) = m.center
    val (dx,dy) = direction
    val len = Math.min(x,y) // 最大探索範囲
    @tailrec
    def lineSearch(i: Int, prev: Int): Int = {
      if (i == len) return i
      else {
        val p = m.probe(x + dx * i, y + dy * i, 100)
        (prev, p) match {
          case (0, 0) => lineSearch(i+1, p)
          case (0, _) => return i
          case (_, 0) => return i
          case (_, _) => lineSearch(i+1, p)
        }
      }
    }
    lineSearch(0, m.probe(x,y, 100))
  }

  def detect: (Int, (Int, Int)) = {
    val (x,y) = m.center
    val l = List(-1, 0 , 1)
    val ds = (for {
      i <- l
      j <- l
    } yield {
      if ((i,j)==(0,0)) (Int.MaxValue, (0,0)) // 0.0は省く
      else (search2((i,j), 2.0), (i,j))
    })
    ds.minBy{case (x, (a,b)) => x}
  }

  // zoopだけ
  def next_zoom: Wander = {
    println(s"zoom")
    Wander(m = m.zoom(1-velocity), direction, velocity)
  }

  def next: Wander = {
    val (len, d) = detect
    print(s"len: $len, ")
    // lenが小さかったらzoomする
    //if (len < Math.max(moveX, moveY)) {
    if (len < 2) {
      println(s"zoom ${d}")
      Wander(m = m.zoom(1-velocity), d, velocity)
    }
    else {
      println(s"move ${d}")
      //Wander(m = moveMandelbrot(d), d, velocity)
      val (x,y) = m.center
      val (dx,dy) = d
      Wander(m = m.move(x+len*dx, y+len*dy).zoom(1-velocity), d, velocity)
    }
  }

  def moveMandelbrot(direction: (Int, Int)): Mandelbrot = {
    val (x,y) = m.center
    m.move(x+moveX*direction._1, y+moveY*direction._2)
  }
}