import org.scalacheck.Arbitrary
import org.specs2.scalaz._
import scalaz.scalacheck.ScalazProperties._

class VecSpec extends Spec {
  implicit val arbitraryVec = Arbitrary {
    for {
      (x, y) <- Arbitrary.arbitrary[(Int, Int)]
    } yield Vec(x, y)
  }

  checkAll(equal.laws[Vec])
  checkAll(monoid.laws[Vec])
}
