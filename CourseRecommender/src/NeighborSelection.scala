
import scala.math._

case class CommonRating(userId1: String, userId2: String, candidate: String, uRating: Double, oRating: Double)

object NeighborSelection extends Task {

  def execute() = {
	 //println("Inside NeighborSelection");
	  
	 DataStore.users.keySet.foreach(user => {
    val neighbors = getUserNeighbours(user, 2) // numNeighbors)

    DataStore.userNeighbours.put(user, neighbors)
    //printList(neighbors)
	 })
	  
	 true
  }
  
  def printList(args: List[_]): Unit = {
    args.foreach(println)
  }
  
  def pearsonSimilarity(ratingsInCommon: Iterable[CommonRating], useAvgRating: Boolean = false): Double = {
     if(ratingsInCommon.size < 2) {
      return 0.0
     }

    var user1SumSquare = 0.0d
    var user2SumSquare = 0.0d
    var user1Sum = 0.0d
    var user2Sum = 0.0d
    var sumSquare = 0.0d

    var commonRatingsTotal = 0

    ratingsInCommon.foreach{ case CommonRating(userId1, userId2, _, myRating, otherRating) =>
      commonRatingsTotal = commonRatingsTotal + 1
      //print("myrating" + myRating + "  otherRating" + otherRating)
      // Sum the squares
      user1SumSquare = user1SumSquare + pow(myRating, 2.0)
      user2SumSquare = user2SumSquare + pow(otherRating, 2.0)
      user1Sum = user1Sum + myRating
      user2Sum = user2Sum + otherRating
      
      // Sum the products
      sumSquare = sumSquare + (myRating * otherRating)
      
    }

    val user1SquareSum = pow(user1Sum, 2.0)
    val user2SquareSum = pow(user2Sum, 2.0)
    
    // Calculate Pearson Correlation score
    val numerator = (commonRatingsTotal * sumSquare) - (user1Sum * user2Sum)
    val denominator = (sqrt((commonRatingsTotal * user1SumSquare) - user1SquareSum) * 
        sqrt((commonRatingsTotal * user2SumSquare) - user2SquareSum))

    denominator match {
      case 0 => 0
      case _ => {
        val res = (1.0 + (numerator / denominator)) / 2.0
      	//val res = (numerator / denominator)
        //println("commonRatingsTotal:" + commonRatingsTotal)
        //println("user2SumSquare" +  user2SumSquare)
        //println("user2SquareSum" +  user2SquareSum)
      	//println("\nY: "+ sqrt((commonRatingsTotal * user2SumSquare) - user2SquareSum))
        //Console.println("N:" + numerator +" D:"+  denominator + " n/d:" + (numerator / denominator) + " Correlation: " + res)
        res * min(commonRatingsTotal/50.0, 1)
      }
    }
  }	
  
  def getUserNeighbours(user: String, numNeighbors: Int) : List[Neighbour] = {
    if(!DataStore.userRatings.contains(user)) {
      //return List()
    }
 
    val myRatings = DataStore.userRatings(user)

    val users: Iterator[String] = DataStore.users.keysIterator
    
    val neighbours = users.filterNot(user.eq).filter(DataStore.userRatings.contains).map { oUserId =>
      val commonItems = DataStore.userRatings(oUserId).view.filter(m => myRatings.contains(m._1))

      val commonRatings = commonItems.map{ case(courseId, oRating) =>
       CommonRating(user, oUserId, oUserId, myRatings.getOrElse(courseId, 0.0), oRating)
      }.toIterable

      Neighbour(oUserId, pearsonSimilarity(commonRatings))
    }.toList

    neighbours.sortBy(n => n.similarity).takeRight(numNeighbors)
  }
 
}