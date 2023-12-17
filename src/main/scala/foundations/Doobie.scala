package foundations

import cats.effect.kernel.MonadCancelThrow
import cats.effect.{IO, IOApp}
import doobie.hikari.HikariTransactor
import doobie.util.transactor.Transactor
import doobie.implicits.*
import doobie.util.ExecutionContexts
object Doobie extends IOApp.Simple {

  case class Student(id: Int, name: String)

  val transactor: Transactor[IO] = Transactor.fromDriverManager(
    "org.postgresql.Driver",
    "jdbc:postgresql:demo",
    "docker",
    "docker"
  )

  def findAllStudents = {
    val query = sql"select name from students".query[String]
    val action = query.to[List]
    action.transact(transactor)
  }

  def insertStudent(id: Int, name: String): IO[Int] = {
    val query = sql"insert into students(id, name) values ($id, $name)"
    val action = query.update.run
    action.transact(transactor)
  }

  def findStudentsByInitial(letter: String) = {
    val selectPart = fr"select id, name"
    val fromPart = fr"from students"
    val wherePart = fr"where left(name, 1) = $letter" //where first 1 simbols equals to first simbols of letter
    val query = selectPart ++ fromPart ++ wherePart
    query.query[Student].to[List].transact(transactor)
  }

  trait Students[F[_]] {
    def findById(id: Int): F[Option[Student]]
    def findALl: F[List[Student]]
    def create(name: String): F[Int]
  }

  object Students {
    def make[F[_]: MonadCancelThrow](transactor: Transactor[F]): Students[F] = new Students[F] {
      override def findById(id: Int): F[Option[Student]] = sql"select id, name from students where id = $id".query[Student].option.transact(transactor)

      override def findALl: F[List[Student]] = sql"select id, name from students".query[Student].to[List].transact(transactor)

      override def create(name: String): F[Int] =
        sql"insert into students(id, name) values $name"
          .update
          .withUniqueGeneratedKeys[Int]("id")
          .transact(transactor)
    }
  }

  private val postgresResource = for {
    ce <- ExecutionContexts.fixedThreadPool[IO](16)
    xa <- HikariTransactor.newHikariTransactor[IO](
      "org.postgresql.Driver",
      "jdbc:postgresql:demo",
      "docker",
      "docker",
      ce
    )
  } yield xa

  val  smallProgram: IO[Unit] = postgresResource.use{ xa =>
    val studentsRepo = Students.make[IO](xa)
    for {
      id <- studentsRepo.create("Daniel")
      daniel <- studentsRepo.findById(id)
      _ <- IO(println(daniel))
    } yield ()
  }
  override def run: IO[Unit]  = findStudentsByInitial("A").map(println)
}
