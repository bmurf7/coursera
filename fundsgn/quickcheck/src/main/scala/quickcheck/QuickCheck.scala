package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(const(empty),genHeap)
  } yield insert(x,h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  // bogus1,bogus2
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // bogus2
  property("hint1") = forAll { (a1: A, a2: A) =>
    findMin(insert(a1,insert(a2,empty))) == ord.min(a1,a2)
  }

  // none
  property("hint2") = forAll { (a: A) =>
    isEmpty(deleteMin(insert(a,empty)))
  }

  // bogus1,bogus2,bogus5
  // recursively findMin deleteMin ensure order
  property("hint3") = forAll { (h: H) =>
    @tailrec
    def iter(last: A, h2: H): Boolean = {
      if (isEmpty(h2)) true
      else if (ord.gt(last, findMin(h2))) false
      else iter(findMin(h2),deleteMin(h2))
    }
    isEmpty(h) || iter(Int.MinValue,h)
  }

  // bogus1,bogus2,bogus5
  property("hint4") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == ord.min(findMin(h1), findMin(h2))
  }

  // bogus3,bogus4
  // changes deleteMin
  // This fails because inserting odd number in reverse order
  // puts the min in ts of t::ts
  property("bogus4") = forAll { (a: A) =>
    val h = insert(3,insert(2,insert(1,empty)))
    findMin(h) != findMin(deleteMin(h))
  }
  // found on githup afterwards
  property("inserting_sequence") = forAll { (l: List[A]) =>
    var h = l.foldRight(empty)(insert)
    toList(h) == l.sorted
  }
  def toList(h:H):List[Int] = if (isEmpty(h)) Nil else findMin(h) :: toList(deleteMin(h))
}
