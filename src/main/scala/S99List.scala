import scala.annotation.tailrec

/**
  * Exercises: http://aperiodic.net/phil/scala/s-99/#lists
  *
  * For the list exercises, avoid using builtin functions such as length, slice, ...
  */
object S99List {
  // P01
  @tailrec
  def last[T](l: List[T]): T = l match {
    case lastElem :: Nil => lastElem
    case _ :: tail => last(tail)
    case Nil => throw new NoSuchElementException("last elem was not found in empty list")
  }

  // P02
  @tailrec
  def penultimate[T](l: List[T]): T = l match {
    case penultimateElem :: _ :: Nil => penultimateElem
    case _ :: tail => penultimate(tail)
    case Nil => throw new NoSuchElementException("penultimate elem was not found in list size < 2")
  }

  // P03
  def nth[T](pos: Int, l: List[T]): T = {
    @tailrec
    def nthRec(pos1: Int, l1: List[T]): T = l1 match {
        case head :: tail if pos1 == 0 => head
        case head :: tail => nthRec(pos1 - 1, tail)
        case Nil => throw new NoSuchElementException(s"$pos is bigger than l length")
      }
    if(pos < 0) throw new IllegalArgumentException(s"$pos is negative")
    else nthRec(pos, l)
  }

  // P04
  def length[T](l: List[T]): Int = {
    @tailrec
    def lengthRec(l: List[T], acc: Int = 0): Int = l match {
      case head :: tail => lengthRec(tail, acc + 1)
      case Nil => acc
    }
    lengthRec(l)
  }

  // P05
  def reverse[T](l: List[T]): List[T] = {
    @tailrec
    def reverseRec(l: List[T], acc: List[T] = Nil): List[T] = l match {
      case head :: tail => reverseRec(tail, head :: acc)
      case Nil => acc
    }
    reverseRec(l)
  }

  // P06
  // implemented with n/2 checks, not n as in solution
  def isPalindrome[T](l: List[T]): Boolean = {
    val length = l.length
    val half = length / 2
    val isEven = half * 2 == length
    val (begin, endWithMiddle) = l.splitAt(half)
    val end = if(isEven) endWithMiddle else endWithMiddle.tail
    begin.equals(end.reverse)
  }

  // P07
  // implemented stack-safe, but slow due ::: usage
  def flatten(l: List[Any]): List[Any] = {
    @tailrec
    def flattenRec(toParse: List[Any], acc: List[Any] = Nil): List[Any] = toParse match {
      case (nestedList: List[Any]) :: tail => flattenRec(nestedList ::: tail, acc)
      case elem :: tail => flattenRec(tail, elem :: acc)
      case Nil => acc
    }
    flattenRec(l).reverse
  }
  // P08
  def compress[T](l: List[T]): List[T] = l.headOption
    .map(head =>
      l.foldLeft[List[T]](List(head)){
        case (acc, elem) =>
          if(acc.head == elem) acc
          else elem :: acc
      }.reverse
    ).getOrElse(l)

  // no need to hold lastElem as argument, but not safe acc.head will be used
//  def compress[T](l: List[T]): List[T] = {
//    @tailrec
//    def compressRec(l: List[T], lastElem: T, acc: List[T]): List[T] = l match {
//      case head :: tail if (head == lastElem) => compressRec(tail, lastElem, acc)
//      case head :: tail => compressRec(tail, head, head :: acc)
//      case Nil => acc
//    }
//    l.headOption.map(head => compressRec(l, head, List(head))).getOrElse(l).reverse
//  }

  // P09
  // seems like a shitcode, but more efficient than original solution (iterate list only 1 time and stack safe)
  def pack[T](l: List[T]): List[List[T]] = {
    if(l.isEmpty) Nil
    else {
      val resReversed: List[List[T]] = l.foldLeft[List[List[T]]](List(List(l.head))) { case (acc, elem) =>
        if (acc.head.head == elem) (elem :: acc.head) :: acc.tail
        else List(elem) :: acc
      }
      val (headWithExtra :: tail) = resReversed.reverse
      headWithExtra.tail :: tail
    }
  }

  // P10
  def encode[T](l: List[T]): List[(Int, T)] = {
    if(l.isEmpty) Nil
    else l.foldLeft[List[(Int, T)]](List((0, l.head))){
      case (acc @ ((count, lastElem) :: tail), elem) =>
        if(elem == lastElem) (count + 1, lastElem) :: tail
        else (1, elem) :: acc
    }.reverse
  }

  // P11
  def encodeModified[T](l: List[T]): List[Any] = {
    encode(l).map {
      case (1, elem) => elem
      case default => default
    }
  }

  // P12
  def decode[T](l: List[(Int, T)]): List[T] = {
    l.flatMap(elem => List.fill(elem._1)(elem._2))
  }

  // P13
  def encodeDirect[T](l: List[T]): List[(Int, T)] = {
    if(l.isEmpty) Nil
    else l.foldLeft[List[(Int, T)]](List((0, l.head))){
      case (acc @ ((count, lastElem) :: tail), elem) =>
        if(elem == lastElem) (count + 1, lastElem) :: tail
        else (1, elem) :: acc
    }.reverse
  }

  // P14
  def duplicate[T](l: List[T]): List[T] = {
    l.flatMap(List.fill(2)(_))
  }

  // P15
  def duplicateN[T](n: Int, l: List[T]): List[T] = {
    l.flatMap(List.fill(n)(_))
  }

  // P16
  def drop[T](n: Int, l: List[T]): List[T] = {
    l.foldLeft[(Int, List[T])]((n - 1, List())){ case ((count, acc), elem) =>
      if(count == 0) (n - 1, acc)
      else (count - 1, elem :: acc)
    }._2.reverse
  }

  // P17

  def split[T](n: Int, l: List[T]): (List[T], List[T]) = {
    @tailrec
    def splitRec(n: Int, acc: List[T], l: List[T]): (List[T], List[T]) = (n, l) match {
      case (0, _)            => (acc.reverse, l)
      case (_, head :: tail) => splitRec(n - 1, head :: acc, tail)
      case (_, Nil)          => (acc.reverse, Nil)
    }

    splitRec(n, List(), l)
  }

  // P18
  @tailrec
  def slice[T](from: Int, to: Int, l: List[T]): List[T] = {
    if(from > 0) slice(from - 1, to - 1, l.tail)
    else l.take(to)
  }

  // P19
  def rotate[T](n: Int, l: List[T]): List[T] = {
    if(l.isEmpty) l
    else {
      val positiveN = (n % l.length + l.length) % l.length
      val (newTail, newHead) = split(positiveN, l)
      newHead ::: newTail
    }
  }

  // P20
  def removeAt[T](n: Int, l: List[T]): (List[T], T) = ???

  // P21
  def insertAt[T](elem: T, n: Int, l: List[T]): List[T] = ???

  // P22
  def range(from: Int, to: Int): List[Int] = ???

  // P23
  def randomSelect[T](n: Int, l: List[T]): List[T] = ???

  // P24
  def lotto(howMany: Int, outOf: Int): List[Int] = ???

  // P25
  def randomPermute[T](l: List[T]): List[T] = ???

  // P26
  def combinations[T](n: Int, l: List[T]): List[List[T]] = ???

  // P27a
  def group3[T](l: List[T]): List[List[List[T]]] = ???

  // P27b
  def group[T](groupSizes: List[Int], l: List[T]): List[List[List[T]]] = ???

  // P28a
  def lsort[T](l: List[List[T]]): List[List[T]] = ???

  // P28b
  def lsortFreq[T](l: List[List[T]]): List[List[T]] = ???
}
