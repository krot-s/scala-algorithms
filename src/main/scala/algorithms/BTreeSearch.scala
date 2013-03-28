import scala.collection.immutable.List
import scala.collection.mutable.Stack
object BTreeSearch extends App {
  implicit def intToNode(value: Int) = new Node(value)

  type BTree = Node
  val tree = new BTree(5)
  tree += 1
  tree += 8
  tree += 6
  tree += 65
  tree += 6
  tree += 12
  tree += 786
  tree += 1
  tree += -45
  tree += 23
  tree += 100
  tree.foreach(println(_))
  println("--------------------------")
  tree.inOrder(x => println(x.value))
}

// TODO change to Ordered
class Node(val value: Int) extends Iterable[Int] {
  private var left: Option[Node] = None
  private var right: Option[Node] = None

  def += (node: Node) {
    if (node.value < value) {
      left match {
        case None => left = Some(node)
        case Some(x) => left.get += node
      }
    } else if (node.value > value) {
      right match {
        case None => right = Some(node)
        case Some(x) => right.get += node
      }
    }
  }

  // traverse tree in order and execute given function 
  def inOrder(f: (Node) => Any): Unit = {
    val stack = Stack[Node]()
    var current = this
    while (current != null) {
      while (current.left.isDefined) {
        stack push current
        current = current.left.get
      }
      f(current)

      while (!current.right.isDefined && !stack.isEmpty) {
        current = stack.pop()
        f(current)
      }
      current = current.right.getOrElse(null)
    }
  }

  override def iterator: Iterator[Int] = {
    var x = this
    new Iterator[Int] {
      val stack = Stack[Node]()
      var current = x
      rewindLeftBranch()

      def rewindLeftBranch() {
        while (current.left.isDefined) {
          stack push current
          current = current.left.get
        }
      }

      override def hasNext = {
        current != null
      }

      override def next = {
        val nx = current

        if (!current.right.isDefined && !stack.isEmpty) {
          current = stack.pop()
        } else {
          current = current.right.getOrElse(null)
          if(current != null) rewindLeftBranch()
        }

        nx.value
      }
    }
  }

  override def toString = {
    val sb = new StringBuilder
    sb append "key = " append value append "\n"
    sb append "   left = " append (if (left.isDefined) left.get.toString else "") append "\n"
    sb append "   right = " append (if (right.isDefined) right.get.toString else "") append "\n"
    sb.toString
  }
} 


