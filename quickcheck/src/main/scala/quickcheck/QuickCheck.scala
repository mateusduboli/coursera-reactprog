package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Gen, Properties}

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("findMin should return the smallest of two values") = forAll { (a: Int, b: Int) =>
    val min = if (a < b) a else b
    val h = insert(a, insert(b, empty))
    findMin(h) == min
  }

  property("deleting the last element should create a empty heap") = forAll { a: Int =>
    val h = insert(a, empty)
    val e = deleteMin(h)
    e == empty
  }

  property("deleting and finding the min should yield a sorted sequence") = forAll { h: H =>
    isSorted(h)
  }

  property("the min of the melding of two heaps, is the min of both") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val g = meld(h1, h2)
    val gMin = if (m1 < m2) m1 else m2
    findMin(g) == gMin
  }

  property("no element can be lost") = forAll { (a: Int, h: H) =>
    val t = insert(a, h)
    exists(a, t)
  }

  def exists(a: Int, h: H): Boolean = {
    if(isEmpty(h)) false
    else findMin(h) == a || exists(a, deleteMin(h))
  }

  def isSorted(h: H) = {
    sortedIteration(findMin(h), h)
  }

  def sortedIteration(a: Int, h: H): Boolean = {
    if(isEmpty(h)) true
    else a <= findMin(h) && sortedIteration(findMin(h), deleteMin(h))
  }

  lazy val genHeap: Gen[H] = for {
    value <- arbitrary[A]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(value, heap)


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
