package jobsboardTest.fixtures

import cats.effect.IO
import jobsboard.core.Users
import jobsboard.domain.user.*

trait UserFixture {

  val mockedUsers: Users[IO] = new Users[IO] {
    override def find(email: String): IO[Option[User]] =
      if (email == danielEmail) IO.pure(Some(Daniel))
      else IO.pure(None)
    override def create(user: User): IO[Either[String, String]] =
      if (user.email == danielEmail) IO.pure(Left("l,popo,"))
      else IO.pure(Right(user.email))
    override def update(user: User): IO[Option[User]] = IO.pure(Some(user))
    override def delete(email: String): IO[Boolean]   = IO.pure(true)
  }

  val Daniel = User(
    "daniel@rockthejvm.com",
    "$2a$10$U3vSFVJuAClZXDtr3A7c/ed0ybdlpSomVUv/ngCR8G45cSwDU.5ti",
    Some("Daniel"),
    Some("Ciocirlan"),
    Some("Rock the JVM"),
    Role.ADMIN
  )
  val danielEmail    = Daniel.email
  val danielPassword = "rockthejvm"

  val Riccardo = User(
    "riccardo@rockthejvm.com",
    "$2a$10$jDPXCNCHkbZLzmiTRuw9A.gBHRDQ1iKnYONCBuskyOln8Aa8eucFa",
    Some("Riccardo"),
    Some("Cardin"),
    Some("Rock the JVM"),
    Role.RECRUITER
  )
  val riccardoEmail    = Riccardo.email
  val riccardoPassword = "riccardorulez"

  val NewUser = User(
    "newuser@gmail.com",
    "$2a$10$6LQt4xy4LzqQihZiRZGG0eeeDwDCvyvthICXzPKQDQA3C47LtrQFy",
    Some("John"),
    Some("Doe"),
    Some("Some company"),
    Role.RECRUITER
  )

  val UpdatedRiccardo = User(
    "riccardo@rockthejvm.com",
    "$2a$10$PUD6CznGVHntJFsOOeV4NezBgBUs6irV3sC9fa6ufc0xp9VLYyHZ.",
    Some("RICCARDO"),
    Some("CARDIN"),
    Some("Adobe"),
    Role.RECRUITER
  )

  val newUserDaniel = NewUserInfo(
    danielEmail, danielPassword, Daniel.firstName, Daniel.lastName, Daniel.company
  )
  val newUserRiccardo = NewUserInfo(
    riccardoEmail, riccardoPassword, Riccardo.firstName, None, None)
}