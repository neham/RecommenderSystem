import io.Source

import scala.math._

object Recommender extends Task {
 def execute() = {
	 //println("Inside Recommender");
	 
	 val testFilename = "resources/test.csv"
	
	 val testRatings = Source.fromFile(testFilename).getLines()
	 testRatings.hasNext match {
	   case true => {
	     val userId = testRatings.next
	     DataStore.users.contains(userId) match {
	       case true => {
	     			val recommendations = userBasedPrediction(userId)
	     			val userName = DataStore.getUserName(userId)
	 
	     			println("Recommendations for user " + userName + ":")
	     			if(recommendations.length != 0) {
	     				recommendations.foreach(x=> println(DataStore.getCourseName(x)))
	     			}
	     			else {
	     				val topReco = topRecommendations()
	     			  topReco.foreach(x=> println(DataStore.getCourseName(x)))
	     			}
	   			}
	       case false => {
	   		     val topReco = topRecommendations()
	   		     println("Recommendations for New User:")
	     			 topReco.foreach(x=> println(DataStore.getCourseName(x)))
	   		}
	     }
	   }
	   case false => println("No Input:")
	 }
	 
	 true  
  }
 
  def printList(args: List[String]): Unit = {
    args.foreach(println)
  }
  
 	def userBasedPrediction(userId: String): List[String] = {
 	
 	  val myRatings = DataStore.userRatings(userId)
 	  val stdDev = calculateStdDeviation(userId)
 	  
 	  stdDev match {
 	    case 0 => {
 	      val topReco = topRecommendations()
 	      topReco
 	    }
 	    case _ => {
 	      val neighbours = DataStore.userNeighbours(userId).filter(n => n.similarity > 0).map(n => n.neighborId -> n.similarity).toMap
 	      val recom = neighbours.map { neighbour =>
 	        
 	      val courses = DataStore.userRatings(neighbour._1).view.filter(m => !myRatings.contains(m._1)).map(n => n._1).toList
 	    
 	      courses
 	      }.toList.flatten.distinct
 	      recom
 	    }
 	  }
 
 	}
 
 	def calculateStdDeviation(userId: String): Double = {
 	  val myRatings = DataStore.userRatings(userId)
 	  val ratingsSum = myRatings.map(m => m._2).toList.sum
 	  val ratingsLength = myRatings.map(m => m._2).toList.length
 	  val ratingsAvg = ratingsSum/ratingsLength
 	  
 	  val diff = (myRatings.map(m => pow((m._2 - ratingsAvg), 2.0)).toList.sum)/ratingsSum
 	  
 	  sqrt(diff)
 	}
 	
 	def parseRecommendations(reco: String) = {
 	  val courseId = reco.trim.split('\t')
 	  courseId(0)
 	} 
 	
 	def topRecommendations(): List[String] ={
 	   val filename = "resources/output.csv"
 	   val lines = Source.fromFile(filename, "utf-8").getLines().withFilter(!_.isEmpty)
 	   val topReco = lines.map(parseRecommendations).toList
 	   topReco
 	}
}
