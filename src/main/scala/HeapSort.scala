import scala.annotation.tailrec

class HeapSort[T](array: Array[T], ordering: Ordering[T]) {

  private def leftIndex(parentIndex: Int, heapLen: Int): Option[Int] = {
    val result = 2*parentIndex + 1
    if (result >= heapLen) None else Some(result)
  }

  private def rightIndex(parentIndex: Int, heapLen: Int): Option[Int] = {
    val result = 2*parentIndex + 2
    if (result >= heapLen) None else Some(result)
  }

  private def swapValues(i: Int, j: Int): Unit = {
    val temp = array(i)
    array(i) = array(j)
    array(j) = temp
  }

  private def indexOfMax(defaultIndex: Int, optIndex: Option[Int]): Int =
    optIndex match {
      case None => defaultIndex
      case Some(otherIndex) =>
        if(ordering.compare(array(otherIndex), array(defaultIndex)) > 0) otherIndex
        else defaultIndex
    }

  @tailrec
  private def maxHeapify(parentIndex: Int, heapLen: Int): Unit = {
    val largestIndex = indexOfMax(
      indexOfMax(parentIndex, rightIndex(parentIndex, heapLen)),
      leftIndex(parentIndex, heapLen))
    if (parentIndex != largestIndex) {
      swapValues(parentIndex, largestIndex)
      maxHeapify(largestIndex, heapLen)
    }
  }

  private def createMaxHeap(heapLen: Int): Unit = {
    for(index <- heapLen/2-1 to 0 by -1) {
      maxHeapify(index, heapLen)
    }
  }

  @tailrec
  private def sortMaxHeap(heapLen: Int): Unit = {
    if (heapLen != 0) {
      swapValues(0, heapLen-1)
      maxHeapify(0, heapLen-1)
      sortMaxHeap(heapLen-1)
    }
  }

  private def sort(): Unit = {
    createMaxHeap(array.length)
    sortMaxHeap(array.length)
  }
}

object HeapSort {

  def sort(array: Array[Int]): Unit = {
    new HeapSort(array, Ordering[Int]).sort()
  }

  def sort[T](array: Array[T], ordering: Ordering[T]): Unit = {
    new HeapSort(array, ordering).sort()
  }
}
