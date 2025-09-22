import Comparador ._
import scala.util.Random

val random = new Random()

def randomList(long:Int): List[Int] ={
  val v = Vector.fill(long){
    random.nextInt(long*2)+1
  }
  v.toList
}

def menorQue(a:Int, b:Int): Boolean = a < b
def mayorQue( a:Int, b:Int ): Boolean = a > b


insert(7, List(2,3,4,8), menorQue)

val iSort_Asc = insertionSort[Int](menorQue)
val iSort_Desc = insertionSort[Int](mayorQue)

iSort_Asc(List(4,5,6,1,2,3))
iSort_Asc(List(3,2,1,6,5,4))


val qSort_Asc = quickSort[Int](menorQue)
val qSort_Desc = quickSort[Int](mayorQue)

qSort_Asc(List( 4, 5, 6, 1, 2, 3))
qSort_Desc(List( 4, 5, 6, 1, 2, 3))

val lAsc100 = (1 to 100).toList
val lDsc100 = (1 to 100).toList.reverse

comparar (iSort_Asc, qSort_Asc, lAsc100)
comparar (iSort_Asc, qSort_Asc, lDsc100)

val lista = List(4, 2, 5, 1)
val pivote = 3
val resultado = menoresQue_noMenoresQue(lista, pivote, menorQue)
