package quickcheck

import org.scalacheck._
import Math.min

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for{
    x <- arbitrary[A]
    h<- oneOf(const(empty),genHeap)
  } yield insert(x,h)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min of 2 elements") = forAll{ (n:A,m:A)=>
    val h = insert(n,insert(m,empty))
    findMin(h)== Math.min(n,m)
  }

  property("insert and delete in empty heap") = forAll { (n:A)=>
    isEmpty(deleteMin(insert(n,empty)))
  }

  property("melding and finding min") = forAll{ (h1:H,h2:H)=>
    findMin(meld(h1,h2)) == Math.min(findMin(h1),findMin(h2))
  }


  property("finding and deleting minima results in sorted seq") = forAll{ (h:H)=>
    def isSorted(h: H):Boolean = {
      (h) match {
        case(empty) => true
        case(h)=>{
          val m = findMin(h)
          val h2 = deleteMin(h)
          isEmpty(h2) || (m <= findMin(h2) && isSorted(h2))
        }
      }
    }
    isSorted(h)
  }

  property("min of 2 heaps should be min after inserting it from heap1 to heap2") = forAll{(h1:H,h2:H)=>
    val min_1 = findMin(h1)
    val min_2 = findMin(h2)
    val m = min(min_1,min_2)
    findMin(meld(deleteMin(h1),insert(min_1,h2))) == m
  }

  property ("2 heaps are equal if finding min and removing elements from both results in same value till empty")
    =forAll{(h1:H,h2:H)=>

    def equalHeap(h1:H,h2:H):Boolean={
      (h1,h2) match{
        case(h1,h2)=>{
          if(isEmpty(h1) && isEmpty(h2)) true
          else if(findMin(h1) != findMin(h2)) false
          else {
            val min_1 = findMin(h1)
            val min_2 = findMin(h2)
            min_1== min_2 && equalHeap(deleteMin(h1),deleteMin(h2))
          }
        }
      }
    }
    equalHeap(meld(h1,h2),meld(deleteMin(h1),insert(findMin(h1),h2)))
  }
}

