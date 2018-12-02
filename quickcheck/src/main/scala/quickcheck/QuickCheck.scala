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
      k <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(k, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll {
    (h: H) =>
      val m = if (isEmpty(h)) 0 else findMin(h)
      findMin(insert(m, h)) == m
  }

  property("min1") = forAll {
    (a: Int) =>
      val h = insert(a, empty)
      findMin(h) == a
  }

  property("min2") = forAll {
    (a: Int, b: Int) =>
      val h1 = insert(a, empty)
      val h2 = insert(b, h1)
      findMin(h2) == Math.min(a, b)
  }

  property("delete min") = forAll {
    (a: Int) =>
      val h = insert(a, empty)
      val hd = deleteMin(h)
      isEmpty(hd)
  }

  property("deleting min sorted seq res") = {
    def removeMin(h: H, acc: Seq[Int]): Seq[Int] = h match {
      case _ if isEmpty(h) => acc
      case _ => removeMin(deleteMin(h), acc :+ findMin(h))
    }

    forAll {
      (h: H) =>
        val seq = removeMin(h, Seq())
        seq == seq.sorted(ord)
    }
  }

  property("minimum of melding") = forAll {
    (h1: H, h2: H) =>
      meld(h1, h2) match {
        case h if isEmpty(h) => true
        case h if isEmpty(h1) => findMin(h) == findMin(h2)
        case h if isEmpty(h2) => findMin(h) == findMin(h1)
        case h => Seq(findMin(h1), findMin(h2)) contains findMin(h)
      }
  }

  property("deleting value leaves heap smaller") = forAll {
    h: H =>
      h match {
        case hp if !isEmpty(hp) => deleteMin(hp) != h
      }
  }

}
