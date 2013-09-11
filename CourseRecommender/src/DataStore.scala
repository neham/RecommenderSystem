
import collection.mutable

import collection.immutable.HashMap


object DataStore {
  
   val users = mutable.HashMap[String, User]()
   val courses = mutable.HashMap[String, Course]()
    
   val userRatings = mutable.HashMap[String, Map[String, Double]]()
   val courseRatings = mutable.HashMap[String, Map[String, Double]]()
   val userNeighbours = mutable.HashMap[String, List[Neighbour]]()
 
   /**
   * Only work if called after all inserts happens
   */
   var avgRatingCourse: Map[String, Double] = _
  
	 def registerRating(rating: Rating, setMovie: Boolean = true) {
    val userRatingMap = userRatings.getOrElse(rating.userId, HashMap[String, Double]())
    userRatings.update(rating.userId, userRatingMap.+( (rating.courseId, rating.rating)  ))
    
    val movieRatingMap = courseRatings.getOrElse(rating.courseId, HashMap[String, Double]())
    courseRatings.update(rating.courseId, movieRatingMap.+((rating.userId, rating.rating)))
   }
   
   def calcCourseAvgRating(): List[String] = {
    avgRatingCourse = courseRatings.map { case(course, ratings) =>
      course -> (ratings.map(_._2).sum/ratings.map(_._2).toList.length)
    }.toMap
    
   //println("Course Average Rating:")
	 //avgRatingCourse.foreach(x=> print(x))

	 val sortReco = avgRatingCourse.toList.sortBy(m => m._2).takeRight(2) 
	 val sortRecoList = sortReco.map(m => m._1)
	  
	 val topRecommendations = sortRecoList.map(x=> getCourseName(x)).toList
	
	 topRecommendations
  }

   def getCourseName(courseId: String): String =  {
      courses.contains(courseId) match {
       case true => { 
      		 courses.get(courseId).get.name
       }
       case _ => null
     }
   }
   
   def getUserName(userId: String): String =  {
     users.contains(userId) match {
       case true => { 
      		 users.get(userId).get.name
       }
       case _ => null
     }
   }
 
}