import quickcheck.{BinomialHeap, IntHeap, Bogus4BinomialHeap}

object A extends Bogus4BinomialHeap with IntHeap {
  val h = insert(2, insert(3, insert(1, empty)))
  val min = findMin(h)
  val d = deleteMin(h)
  val m = findMin(d)
  val s = m >= min
}

object B extends BinomialHeap with IntHeap {
  val h = insert(2, insert(3, insert(1, empty)))
  val min = findMin(h)
  val d = deleteMin(h)
  val m = findMin(d)
  val s = m >= min
}

A.h
A.min
A.d
A.m
A.s

B.h
B.min
B.d
B.m
B.s

