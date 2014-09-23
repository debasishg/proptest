package net.debasishg.prop

import org.scalacheck._
import Prop.{ forAll, BooleanOperators }

object ListSpecification extends Properties("List") {

  property("double reverse gives original") = forAll { (lst: List[Int]) =>
    lst.reverse.reverse == lst
  }

  property("concat preserves size") = forAll { (l1: List[Int], l2: List[Int]) =>
    l1.size + l2.size == (l1 ::: l2).size 
  }

  val smallInteger = Gen.choose(0,100)
  property("make list of length n") = forAll(smallInteger) { n =>
    List.fill(n)("").length == n
  }

  property("make list of length n with implication") = forAll { n: Int =>
    (n >= 0 && n <= 1000) ==> (List.fill(n)("").length == n)
  }
}

object StringSpecification extends Properties("String") {

  property("concat string preserves order") = forAll { (s1: String, s2: String) =>
    (s1 + s2).endsWith(s2)
  }
}

object Generators extends Properties("Generators") {
  val smallInteger = Gen.choose(0,100)
  property("integers in range") = forAll(smallInteger)(n => n >= 0 && n <= 100)
}

