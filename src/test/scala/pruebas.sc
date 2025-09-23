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

val emptyList = List()
val singleList = List(5)
val equalList = List(5,5,5,5,5)
val ascList = List(1,2,3,4,5)
val descList = List(5,4,3,2,1)
val mixList = List(5,1,4,3,2)
val charList = List('j','d','c','m')
val stringList = List("Liseth", "Natalia", "Rivera", "Cordoba")

insert[Int](2, emptyList, menorQue)
insert[Int](2, singleList, menorQue)
insert[Int](2, ascList, mayorQue)
insert[Int](2, descList, menorQue)
insert[Char]('f', List('a', 'b', 'c', 'd'), (a,b)=>a<b)
insert[String]("Efe", List("A", "Be", "Ce", "De"), (a,b)=>a<b)


val iSort_Asc = insertionSort[Int](menorQue)
val iSort_Desc = insertionSort[Int](mayorQue)
val iSort_Asc_Anon = insertionSort[Int]((a,b)=>a<b)
val iSort_Desc_Anon = insertionSort[Int]((a,b)=>a>b)
val iSort_Eq = insertionSort[Int]((a,b)=> a==b)
val iSort_Asc_Char = insertionSort[Char]((a,b)=>a<b)
val iSort_Desc_Char = insertionSort[Char]((a,b)=>a>b)
val iSort_Asc_Str = insertionSort[String]((a,b)=>a<b)
val iSort_Desc_Str = insertionSort[String]((a,b)=>a>b)

iSort_Asc(emptyList)
iSort_Asc(singleList)
iSort_Asc(equalList)
iSort_Asc(ascList)
iSort_Asc(descList)
iSort_Asc(mixList)
iSort_Asc_Anon(mixList)

iSort_Desc(emptyList)
iSort_Desc(singleList)
iSort_Desc(equalList)
iSort_Desc(ascList)
iSort_Desc(descList)
iSort_Desc(mixList)
iSort_Desc_Anon(mixList)

iSort_Asc_Char(charList)
iSort_Desc_Char(charList)

iSort_Asc_Str(stringList)
iSort_Desc_Str(stringList)

menoresQue_noMenoresQue[Int](singleList, 5, menorQue)
menoresQue_noMenoresQue[Int](ascList, 3, menorQue)
menoresQue_noMenoresQue[Int](descList, 3, menorQue)
menoresQue_noMenoresQue[Int](mixList, 3, menorQue)
menoresQue_noMenoresQue[Char](charList, 'd', (a,b)=>a<b)
menoresQue_noMenoresQue[String](stringList, "Natalia", (a,b)=>a<b)

val qSort_Asc = quickSort[Int](menorQue)
val qSort_Desc = quickSort[Int](mayorQue)
val qSort_Asc_Anon = quickSort[Int]((a,b)=>a<b)
val qSort_Desc_Anon = quickSort[Int]((a,b)=>a>b)
val qSort_Eq = quickSort[Int]((a,b)=> a==b)
val qSort_Asc_Char = quickSort[Char]((a,b)=>a<b)
val qSort_Desc_Char = quickSort[Char]((a,b)=>a>b)
val qSort_Asc_Str = quickSort[String]((a,b)=>a<b)
val qSort_Desc_Str = quickSort[String]((a,b)=>a>b)

qSort_Asc(emptyList)
qSort_Asc(singleList)
qSort_Asc(equalList)
qSort_Asc(ascList)
qSort_Asc(descList)
qSort_Asc(mixList)
qSort_Asc_Anon(mixList)

qSort_Desc(emptyList)
qSort_Desc(singleList)
qSort_Desc(equalList)
qSort_Desc(ascList)
qSort_Desc(descList)
qSort_Desc(mixList)
qSort_Desc_Anon(mixList)

qSort_Asc_Char(charList)
qSort_Desc_Char(charList)

qSort_Asc_Str(stringList)
qSort_Desc_Str(stringList)

val lAsc100 = (1 to 100).toList
val lDsc100 = (1 to 100).toList.reverse

comparar(iSort_Asc, qSort_Asc, mixList)
comparar(iSort_Asc, qSort_Desc, mixList)
comparar (iSort_Asc, qSort_Asc, lAsc100)
comparar (iSort_Asc, qSort_Asc, lDsc100)


val l200=randomList(200)
val l500=randomList(500)

iSort_Asc(l200)
iSort_Desc(l500)

qSort_Asc(l200)
qSort_Desc(l500)

comparar(iSort_Asc, qSort_Asc, l200)
comparar(iSort_Asc, qSort_Asc, l500)
