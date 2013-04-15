/**
 * Bubble sort in imperative and functional ways
 */
package algorithms

import scala.collection.mutable.ListBuffer

object BubbleSort extends App {
  val xs = List(5, 4, 3, 2, 1, 5, 1, 2, 4, 10)
  println(bubbleSort(xs))
  println(bubbleSortFunctional(xs))

  def bubbleSort[T <% Ordered[T]](xs: List[T]): List[T] = {
    val xz = ListBuffer[T](xs: _*)
    var i = 0
    while (i < xz.length - 1) {
      if (xz(i) > xz(i + 1)) {
        swap(xz, i, i + 1)
        i = 0
      } else i += 1
    }
    xz.toList
  }

  def bubbleSortFunctional[T <% Ordered[T]](xs: List[T]): List[T] = {
    def sort[T <% Ordered[T]](in: ListBuffer[T], n: Int, j: Int) {
      if (in(n) > in(j)) {
        swap(in, n, j)
        sort(in, 0, 1)
      } else if (j < in.length - 1)
        sort(in, n + 1, j + 1)
    }

    if (xs.isEmpty) xs else {
      val xz = ListBuffer[T](xs: _*)
      sort(xz, 0, 1)
      xz.toList
    }
  }

  private def swap[T](xs: ListBuffer[T], i: Int, y: Int) {
    val t = xs(i)
    xs(i) = xs(y)
    xs(y) = t
  }
}