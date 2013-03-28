class BubbleSort {
	def bubbleSort(array : Array[Int]) : Array[Int] = {
		var i = 0
		while(i < array.length - 1){
		  if (array(i) > array(i + 1)) {
		    val s = array(i)
		    array(i) = array(i+1)
		    array(i+1) = s
		    i = 0
		  } else i += 1
		}
		array;
	}

	def bubbleSortFunctional(in : Array[Int]) : Array[Int] = {
	  val array = in.clone()
		sort(array, 0, 0 max 1, false)
		array;
	}
	
	
	private def sort(in : Array[Int], n : Int, j : Int, swapped : Boolean) {
	  val changed = in(n) > in(j) 
	  if(changed){
	    val temp = in(n)
	    in(n) = in(j)
	    in(j) = temp
	  }
	  
	  if(j < in.length - 1){
	    sort(in, n + 1, j + 1, swapped || changed)
	  } else {
		  if(swapped || changed) {
			  sort(in, 0, 0 max 1, false)
		  }
	  }
	}

}