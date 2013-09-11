
object CourseRecom extends App {
	 val defaultPipe = List (
	  ("CParser" -> CParser),
    ("NeighborSelection" -> NeighborSelection),
    ("Recommender" -> Recommender)
   )
  
   
	 defaultPipe.takeWhile{ case (pipeName, task) =>
      task.execute()
   }
	 
}