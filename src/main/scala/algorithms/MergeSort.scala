package algorithms

import scala.collection.mutable.ListBuffer
import scala.util.Random
import java.util.Collections
import scala.collection.JavaConversions._
import java.util.Comparator
import java.util.ArrayList

object MergeSort extends App {
  println(sort(List(5, 4, 3, 2, 1, 5, 1, 2, 4, 10)))

  def sort[T <% Ordered[T]](xs: List[T]): List[T] = {
    def merge[T <% Ordered[T]](l: List[T], r: List[T]): List[T] = (l, r) match {
      case (_, Nil) => l
      case (Nil, _) => r
      case (x :: xs, y :: ys) => {
        if (x < y) x :: merge(xs, r)
        else y :: merge(l, ys)
      }
    }
    val middle = xs.length / 2
    if (middle == 0) xs
    else merge(sort(xs take middle), sort(xs drop middle))
  }
}