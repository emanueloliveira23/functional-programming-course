package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      v <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(v, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // Following hints

  property("min1") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == math.min(a, b)
  }

  property("empty1") = forAll { a: Int =>
    val h = insert(a, empty)
    val d = deleteMin(h)
    d == empty
  }

  property("order1") = forAll { h: H =>

    def compareMin(h: H): Boolean = {
      if (isEmpty(h)) true
      else {  
        val rest = deleteMin(h)
        isEmpty(rest) || ((findMin(h) <= findMin(rest)) && compareMin(rest))
      }
    }

    compareMin(h)
  }

  property("min2") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) && isEmpty(h2)) true
    else {
      val min1 = if (isEmpty(h1)) 0 else findMin(h1)
      val min2 = if (isEmpty(h2)) 0 else findMin(h2)
      findMin(meld(h1, h2)) == math.min(min1, min2)
    }

  }

}