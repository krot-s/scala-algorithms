import scala.io.Source
import java.io.File
import java.io.IOException


object AStar extends App {
	sealed abstract class Token
	case class IfKeyword extends Token
	case class ThenKeyword extends Token
	case class ElseKeyword extends Token
	case class NumberLiteral(value: String) extends Token
	case class VariableLiteral(value: String) extends Token
	case class StringLiteral(value: String) extends Token
	case class UsingExpression(value: VariableLiteral) extends Token

	val ik = IfKeyword
	val variable = VariableLiteral("abc")
	val second = variable

	desc(UsingExpression(VariableLiteral("bf")))
	
	def desc(token: Token) {
	  token match {
	    case IfKeyword() => println("If")
	    case ThenKeyword() => println("Then")
	    case ElseKeyword() => println("Else")
	    case NumberLiteral(value) => println("Number " + value)
	    
	    case UsingExpression(VariableLiteral(b)) => println("USING!!! " + b)
	    
	    case _ => println("unexpected")
	  }
	}
  
	/*try {
		val s = Source.fromFile(new File("/home/slava/123.html"))
		val delimiterLine = s.getLine(0)
		println(delimiterLine)
		for(x <- s.getLines()){
		  x match {
		    case `delimiterLine` => println("new block")	
		    case x => println(x) 
		  }
		}
		
		val result = 
			"e" match {
			  case "1" => "one"
			  case "e" => "ee"
			  case _ =>
			}
		println(result)
		
	} catch {
	  case e: IOException => {
	    println(e)
	  }
	}*/
}