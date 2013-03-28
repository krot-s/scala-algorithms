package graph

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.PriorityQueue
object GraphTest extends App {
	var graph = new Graph	
	val n1 = graph.makeNode(1)
	val n2 = graph.makeNode(2)
	val n3 = graph.makeNode(3)
	val n4 = graph.makeNode(4)
	val n5 = graph.makeNode(5)
	val n6 = graph.makeNode(6)
	val n7 = graph.makeNode(7)
	
	graph.add(n1)
	n1.connect(n1, 1)
	n1.connect(n2, 4)
	n1.connect(n3, 2)
	n1.connect(n4, 2)
	
	n2.connect(n5, 1)
	
	n3.connect(n1, 1)
	n3.connect(n2, 1)
	n3.connect(n7, 5)
	
	n5.connect(n6, 1)
	n5.connect(n3, 1)
	
	n6.connect(n4, 1)
	n6.connect(n7, 1)
	println(graph.dijkstraMinDistance(n1, n7))
//	println(graph)
}