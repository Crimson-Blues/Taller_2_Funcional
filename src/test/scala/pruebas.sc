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

insert(7, List(2,3,4,8), menorQue)

val iSort = insertionSort[Int](menorQue)

iSort(List(4,5,6,1,2,3))
iSort(List(3,2,1,6,5,4))