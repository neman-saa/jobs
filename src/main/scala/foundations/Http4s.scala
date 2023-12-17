package foundations

import cats.MonadThrow
import cats.effect.{IO, IOApp}
import cats.effect.Concurrent
import org.http4s.{EntityDecoder, HttpRoutes}
import org.http4s.dsl.Http4sDsl
import org.http4s.dsl.impl.{OptionalValidatingQueryParamDecoderMatcher, QueryParamDecoderMatcher}
import io.circe.generic.auto.*
import org.http4s.circe.*
import io.circe.syntax.*
import org.http4s.ember.server.EmberServerBuilder
import cats.implicits.*
import cats.*
import org.http4s.FormDataDecoder.formEntityDecoder

import java.util.UUID

object Http4s extends IOApp.Simple {
  //simulate an http server with students and courses

  type Student = String

  case class Instructor(name: String, secondName: String)

  case class Course(id: String, title: String, year: Int, students: List[Student], instructor: String)

  object CourseRepository {

    private val catsEffectCourse = Course(
      "2360ed21-b59d-4f7a-bf27-4db97abcc748",
      "Rite of passage",
      2022,
      List("Daniel", "Master Yoda"),
      "Martin Odersky"
    )
    private val courses: Map[String, Course] = Map(catsEffectCourse.id -> catsEffectCourse)

    def findCourseById(courseID: UUID): Option[Course] =
      courses.get(courseID.toString)

    def findCoursesByInstructor(name: String): List[Course] =
      courses.values.filter(_.instructor == name).toList
  }

  object InstructorQueryParamMatcher extends QueryParamDecoderMatcher[String]("instructor")

  object YearQueryParamMatcher extends OptionalValidatingQueryParamDecoderMatcher[Int]("year")

  def courseRoute[F[_] : Concurrent]: HttpRoutes[F] = {
    val dsl = Http4sDsl[F]
    import dsl.*

    HttpRoutes.of[F] {
      case GET -> Root / "courses" :? InstructorQueryParamMatcher(instructor) +& YearQueryParamMatcher(mbYear) =>
        val courses = CourseRepository.findCoursesByInstructor(instructor)
        mbYear match {
          case Some(year) => year.fold(
            _ => BadRequest("Parameter year is invalid"),
            year => Ok(courses.filter(_.year == year).asJson)
          )
          case None => Ok(courses.asJson)
        }
      case GET -> Root / "courses" / UUIDVar(courseId) / "students" =>
        CourseRepository.findCourseById(courseId).map(_.students) match {
          case Some(students) => Ok(students.asJson)
          case None => NotFound(s"No courses with id $courseId was found")
        }
    }
  }

  class TestRoutes[F[_] : Concurrent] extends Http4sDsl[F] {
    implicit val decoder: EntityDecoder[F, Course] = jsonOf[F, Course]
    val testRoute: HttpRoutes[F] = HttpRoutes.of[F] {
      case req @ POST -> Root / "test" =>
        for {
          course <- req.as[Course]
          resp <- Ok(s"Course with id ${course.id} was added")
        } yield resp
    }
  }

  object TestRoutes {
    def apply[F[_] : Concurrent]() = new TestRoutes[F]
  }

  def allRoutes[F[_] : Concurrent] = courseRoute[F] <+> courseRoute[F]

  override def run: IO[Unit] = EmberServerBuilder.default[IO].withHttpApp(allRoutes[IO].orNotFound).build.use(_ => IO.println("ServerStarted") *> IO.never)

}
