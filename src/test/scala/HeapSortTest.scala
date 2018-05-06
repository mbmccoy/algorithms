import org.junit.Test
import org.junit.Assert._


class HeapSortTest {

  @Test
  def testOddLenSort(): Unit = {
    val arrayOdd = Array(-3, 12, 1, 1, 7, -9, 2)
    HeapSort.sort(arrayOdd)
    assertEquals(
      Array(-9, -3, 1, 1, 2, 7, 12).mkString(", "),
      arrayOdd.mkString(", "))
  }

  @Test
  def testEvenLenSort(): Unit = {
    val arrayEven = Array(-3, 12, 1, 1, 7, -9)
    HeapSort.sort(arrayEven)
    assertEquals(
      Array(-9, -3, 1, 1, 7, 12).mkString(", "),
      arrayEven.mkString(", "))
  }

  @Test
  def testEmptySort(): Unit = {
    val empty = Array.empty[Int]
    HeapSort.sort(empty)
    assertEquals(
      Array.empty[Int].mkString(", "),
      empty.mkString(", "))
  }

  @Test
  def testReverseSort(): Unit = {
    val array = Array(0.1, 0.2, 0.3, 0.4, 0.0, -0.1)
    HeapSort.sort(array, Ordering.by[Double, Double](-_))
    assertEquals(
      Array(0.4, 0.3, 0.2, 0.1, 0.0, -0.1).mkString(", "),
      array.mkString(", "))
  }
}
