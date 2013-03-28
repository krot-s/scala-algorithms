object InsertionSort extends App {
  println(sort(List(5, 4, 3, 2, 1, 5, 1, 2, 4, 10)))
  println(isort(List(5, 4, 3, 2, 1, 5, 1, 2, 4, 10)))

  def sort(xs: List[Int]): List[Int] = xs match {   
    case Nil => Nil
    case List(x) => xs
    case x :: y => {
      if (x > y.head) y.head :: sort(x :: sort(y.tail))
      else x :: sort(y)
    }
  }

  def isort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) Nil
    else insert(xs.head, isort(xs.tail))
  def insert(x: Int, xs: List[Int]): List[Int] =
    if (xs.isEmpty || x <= xs.head) x :: xs
    else xs.head :: insert(x, xs.tail)

}