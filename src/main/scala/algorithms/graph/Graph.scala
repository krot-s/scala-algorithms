package graph

import scala.collection.mutable.HashSet
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedList
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.PriorityQueue
class Graph {
	var nodes = new HashSet[Node]
	
	def this(nodes : HashSet[Node]){
	  this
	  this.nodes = nodes
	}
	
	def add(node : Node) {
	  nodes += node
	}
	
	def remove(node : Node) {
	  nodes -= node
	}
	
	def dijkstraMinDistance(start: Node, target: Node): List[Node] = {
	  var toVisit = new PriorityQueue[Tuple3[Node, Int, ListBuffer[Node]]]()(InverseOrder)
	  toVisit += ((start, 0, ListBuffer(start)))
	  
	  var dest: ListBuffer[Node] = null
	  while(toVisit.size > 0 && dest == null){
		  val t = toVisit.dequeue()
		  val node = t._1
		  // weight of current node
		  val weight = t._2
		  // path to current node
		  val path = t._3
		  
		  if(node == target){
		  	dest = path
		  }	else {
			  for((n, w) <- node.connections) {
			    var current = Integer.MAX_VALUE
			    toVisit.foreach(x => {
			    	if (x._1 == n) {			    		
			    		current = x._2 min current
			    	}
			    })
				    
			    if(weight + w < current){
			      // TODO uncomment this
			    	// toVisit += ((n, weight + w, path.clone() + n ))
			    }
			  }
		  }
			  
	  }		  
		  			  
	  dest.toList
	}
	
	object InverseOrder extends Ordering[Tuple3[Node, Int, ListBuffer[Node]]] {
		def compare(a: Tuple3[Node, Int, ListBuffer[Node]], b: Tuple3[Node, Int, ListBuffer[Node]]) = {
		  if (a._2 > b._2) {
		    -1
		  } else if(a._2 < b._2) {
		    1
		  } else {
		    0
		  }
		}
	}
	
	override def toString() = {
	  val sb = new StringBuilder
	  def nodeToString(node : Node) = {
		  val sb = new StringBuilder
		  sb.append(node.value)
		  		  
		  if(node.connections.size > 0){
			  sb.append(" => ")
			  node.connections.foreach {
			    case (key, value) => 
			      sb.append("[value=" + key.value + ",weight=" + value + "]")
			      if((key, value) != node.connections.last) sb.append(",")		      
			  }
		  }
		  sb.toString()
	  }
	  	  
	  for(node <- nodes){
		  sb.append(nodeToString(node) + "\n")
	  }
	  sb.toString()
	}
	
	def makeNode(value : Int) = {
		new Node(value, this)
	}
}

class Node(v : Int, graph : Graph) {
	var connections = Map[Node, Int]()

	def value = v

	def connect(node : Node) {
	  connect(node, 1)
	}
	
	def connect(node : Node, weight : Int) {
	  graph.add(node)
	  connections += (node -> weight)
	}
	
	override def toString = "'" + v.toString() + "'"
}	  

