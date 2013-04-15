package algorithms

object InsertionSort extends App {
  println(sort(List(5, 4, 3, 2, 1, 5, 1, 2, 4, 10)))

  def sort[T <% Ordered[T]](xs: List[T]): List[T] = xs match {
    case Nil => Nil
    case List(x) => xs
    case x :: y => insert(x, sort(y))
  }

  private def insert[T <% Ordered[T]](x: T, sorted: List[T]): List[T] = {
    if (sorted.isEmpty || x <= sorted.head) x :: sorted
    else sorted.head :: insert(x, sorted.tail)
  }
}