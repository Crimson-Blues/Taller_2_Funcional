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
          print("Insertando: ")
          println(lUnorder.head)
          print("Se usaron: ")
          println(insertStep._2)
          insertionSortIter(insertStep._1, lUnorder.tail, insertStep._2 + totalOps)
        }
      }
      insertionSortIter(List(l.head), l.tail, 0)
      }

    insertionSortInstance
    }


}
