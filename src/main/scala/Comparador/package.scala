import scala.annotation.tailrec

package object Comparador {
  type AlgoritmoOrd[T] = List[T] => (List[T], Int)
  type Comparador[T] = (T,T) => Boolean

  def insert[T](e:T, l:List[T], comp:Comparador[T]): (List[T], Int) = {

    @tailrec
    def insertIter(left:List[T], right:List[T], ops:Int): (List[T], Int) = {
      if (right.isEmpty) (left ++ List(e), ops)
      else {
        if (comp(e, right.head)) (left ++ (e :: right), ops + 1)
        else insertIter(left ++ List(right.head), right.tail, ops + 1)
      }
    }

    insertIter(List(), l, 0)
  }

  def insertionSort[T](comp:Comparador[T]): AlgoritmoOrd[T] = {
    def insertionSortInstance (l: List[T]): (List[T], Int) = {
      @tailrec
      def insertionSortIter(lOrder:List[T], lUnorder:List[T], totalOps:Int): (List[T], Int) = {
        if (lUnorder.isEmpty) (lOrder, totalOps)
        else {
          val insertStep = insert(lUnorder.head, lOrder, comp)
          insertionSortIter(insertStep._1, lUnorder.tail, insertStep._2 + totalOps)
        }
      }
      insertionSortIter(List(l.head), l.tail, 0)
      }

    insertionSortInstance
    }

  def comparar[T](a1:AlgoritmoOrd[T], a2:AlgoritmoOrd[T], l:List[T]):(Int, Int) = {
    val (l1, c1) = a1(l)
    val (l2, c2) = a2(l)

    if(l1==l2) (c1,c2) else (-1,-1)
  }


}
