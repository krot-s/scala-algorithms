object FindFrequentElement extends App {
	val elements = Array(1,2,1,2,1,2,1,2,1,2,1,3,1,3,1,1,1,1,3,3)
	var candidate = elements(0);
	var standing = 0
	for(x <- elements){
		println("c = " + candidate)
		if(standing == 0){
		  println("x = " + x)
		  candidate = x
		  standing = standing + 1
		} else {
		  println("adding " + (if (candidate == x) 1 else -1))
		  standing = standing + (if (candidate == x) 1 else -1)
		}
	}
	
	println(candidate)
}