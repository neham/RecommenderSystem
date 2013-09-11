import io.Source
import java.util.concurrent.atomic.AtomicInteger

case class Course(courseId: String, name: String, Specialization: String)
case class Rating(userId: String, courseId: String, rating: Double)
case class User(userId: String, name: String, course: String)
case class Neighbour(neighborId: String, similarity: Double)

object CParser extends Task {
	def execute() = {
		//println("Inside CParser");

		val ratingFileName = "resources/Ratings.csv"
		val usersFileName = "resources/users.csv"
		val coursesFileName = "resources/courses.csv"

		importUsers(usersFileName);
		importCourses(coursesFileName);
		importRatings(ratingFileName);
		true
	}

	def importUsers(file: String) {
    val lines = Source.fromFile(file, "utf-8").getLines().withFilter(!_.isEmpty)
    lines.map(parseUser).toList.foreach( user => {
      DataStore.users.put(user.userId, user)

      //Console.println("Import user " + user.userId)
    })
  }
	
	def importCourses(file: String) {
		val lineCount = new AtomicInteger(0)
		
		val lines = Source.fromFile(file, "utf-8").getLines().withFilter(!_.isEmpty)
		
		// process in parallel each course
		lines.map(parseCourse).foreach { case course =>
			DataStore.courses.put(course.courseId, course)
			// TODO
			//DataStore.movieRatings.put(movie.id, Map[String, Double]())
			
			//Console.println("finished course - count: " + lineCount.incrementAndGet())
		}
  }
	 
	def importRatings(file: String) {
    val count = new AtomicInteger(0)

    // load all ratings to memory to faster parallel processing
    val lines = Source.fromFile(file, "utf-8").getLines().withFilter(!_.isEmpty)

    lines.map(parseRating).foreach{ rating =>
      DataStore.registerRating(rating)

      count.incrementAndGet()

      //Console.println("finished rating - count: " + count.get)
    }

    //DataStore.calcUserAvgRating()
  }
	
	def importTestRatings(file: String) {
		val lines = Source.fromFile(file, "utf-8").getLines().withFilter(!_.isEmpty)
				lines.foreach(f => parseRating(f))

	}

	def parseCourse(courseLine: String) = {
		val course = courseLine.split(",")
				Course(course(0), course(1), course(2))
	}

	def parseRating(ratingLine: String) = {
		val rating = ratingLine.trim.split(',')
				Rating(rating(0), rating(1), rating(2).toDouble)
	}

	def parseUser(userLine: String) = {
		val user = userLine.split(',')
				User(user(0), user(1), user(2))
	}
	
	def parseTestData(userLine: String) = {
		
	}
}
