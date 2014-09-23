import scalaz._

object Vec {
  implicit object VecEqual extends Equal[Vec] {
    def equal(v1: Vec, v2: Vec): Boolean = v1 == v2
  }

  implicit object VecMonoid extends Monoid[Vec] {
    def zero: Vec = Vec(0, 0)
    def append(v1: Vec, v2: => Vec): Vec = Vec(v1.x + v2.x, v1.y + v2.y)
  }
}

case class Vec(x: Int, y: Int)
