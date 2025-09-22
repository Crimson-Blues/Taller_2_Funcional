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
    def insertionSortInstance(l: List[T]): (List[T], Int) = {
      if (l.isEmpty) (l, 0)
      else {
        val (sortedTail, opsTail) = insertionSortInstance(l.tail)
        val (listStep, numOps) = insert(l.head, sortedTail, comp)

        (listStep, numOps + opsTail)
      }
    }

    insertionSortInstance
    }


  def menoresQue_noMenoresQue[T](l: List[T], v: T, comp: Comparador[T]): (List[T], List[T], Int) = {
    @tailrec
    def partitionIter(rest: List[T], menores: List[T], noMenores: List[T], ops: Int): (List[T], List[T], Int) = {
      if (rest.isEmpty) (menores, noMenores, ops)
      else {
        val head = rest.head
        val tail = rest.tail
        if (comp(head, v))
          partitionIter(tail, head :: menores, noMenores, ops + 1)
          //partitionIter(tail, menores ++ List(head), noMenores, ops + 1)
        else
          partitionIter(tail, menores, head :: noMenores, ops + 1)
          //partitionIter(tail, menores, noMenores ++ List(head), ops + 1)
      }
    }
    partitionIter(l, List(), List(), 0)
  }


  def quickSort[T](comp: Comparador[T]): AlgoritmoOrd[T] = {
    def quickSortInstance(l: List[T]): (List[T], Int) = {
      if (l.isEmpty || l.tail.isEmpty) (l, 0)
      else {
        val pivot = l.head
        val rest = l.tail
        val (menores, noMenores, opsPartition) = menoresQue_noMenoresQue(rest, pivot, comp)
        val (sortedMenores, opsMenores) = quickSortInstance(menores)
        val (sortedNoMenores, opsNoMenores) = quickSortInstance(noMenores)
        (sortedMenores ++ List(pivot) ++ sortedNoMenores, opsPartition + opsMenores + opsNoMenores)
      }
    }
    quickSortInstance
  }

  def comparar[T](a1: AlgoritmoOrd[T], a2: AlgoritmoOrd[T], l: List[T]): (Int, Int) = {
    val (l1, c1) = a1(l)
    val (l2, c2) = a2(l)

    if (l1 == l2) (c1, c2) else (-1, -1)
  }

}
