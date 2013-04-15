/**
 * Find most frequent element with Boyer-Moore algorithm
 */
package algorithms

object FindFrequentElement extends App {
	val xs = Array(1,2,3,1,1)
	println(mostFrequentElement(xs : _*))
	
	def mostFrequentElement[T](xs: T*) = {
		var candidate = xs(0);
		var standing = 0
		for(x <- xs){
			if(standing == 0){
			  candidate = x
			  standing = standing + 1
			} else {
			  standing = standing + (if (candidate == x) 1 else -1)
			}
		}
		if(standing > 0) Some(candidate) else None
	}
}