package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    for {
      k <- arbitrary[Int]
      m <- oneOf[H](empty, genHeap)
    } yield insert(k, m)
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  /*
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
 */

  // 2
  property("gen2") = forAll { (p: (Int, Int)) =>
    val min = Math.min(p._1, p._2)
    findMin(insert(p._1, insert(p._2, empty))) == min && findMin(insert(p._2, insert(p._1, empty))) == min
  }


  property("gen3") = forAll { (p: Int) =>
    deleteMin(insert(p, empty)) == empty
  }


  // 5 & 1
  property("gen4") = forAll { (h: H) =>

    def checkSeq(heap: H, prev: Int): Boolean = {
      if (heap == empty) true
      else {
        val min = findMin(heap)
        if (min < prev) false
        else
          checkSeq(deleteMin(heap), min)
      }
    }

    checkSeq(h, Int.MinValue)
  }

  // 5
  property("gen5") = forAll { (heaps: (H, H)) =>
    findMin(meld(heaps._1, heaps._2)) == Math.min(findMin(heaps._1), findMin(heaps._2))
  }
  // 6
  property("gen6") = forAll { (h: H) =>

    def getElems(heap: H): List[A] = {
      if (isEmpty(heap)) Nil else {
        findMin(heap) :: getElems(deleteMin(heap))
      }
    }

    val listElems = getElems(h)

    def generateListShuffle(l: List[Int], heapList: List[A]): List[A] = (l, heapList) match {
      case (Nil, list) => list
      case (_, Nil) => Nil
      case (h1 :: h2 :: t, _) => {
        if (h1 > h2)
          heapList.head :: generateListShuffle(t, heapList.tail)
        else
          heapList.last :: generateListShuffle(t, heapList.init)
      }
      case (_ :: _, _) => heapList
    }

    forAll { (l: List[Int]) =>

      val newHeap = generateListShuffle(l, listElems).foldLeft(empty)((x, y) => insert(y, x))
      val newList = getElems(newHeap)
      listElems == newList
    }
  }
}
