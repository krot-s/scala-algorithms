package algorithms

import scala.collection.mutable.ListBuffer
import scala.util.Random
import java.util.Collections
import scala.collection.JavaConversions._
import java.util.Comparator
import java.util.ArrayList

object QuickSort extends App {
  println(sort(List(5, 4, 3, 2, 1, 5, 1, 2, 4, 10)))

  def sort[T <% Ordered[T]](list: List[T]): List[T] = {
    list match {
      case Nil => Nil
      case x :: xs =>
        val (before, after) = xs partition (_ < x)
        sort(before) ++ (x :: sort(after))
    }
  }
}