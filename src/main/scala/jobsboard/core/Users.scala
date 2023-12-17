package jobsboard.core

import cats.effect.kernel.MonadCancelThrow
import doobie.Transactor
import jobsboard.domain.user.User
import org.typelevel.log4cats.Logger
import cats.implicits.*
import doobie.*
import doobie.util.*
import doobie.implicits.*
import cats.*
import cats.effect.*

trait Users[F[_]] {
  def find(email: String): F[Option[User]]
  def create(user: User): F[Either[String, String]]
  def update(user: User): F[Option[User]]
  def delete(email: String): F[Boolean]
}

final class LiveUsers[F[_]: MonadCancelThrow: Logger](xa: Transactor[F]) extends Users[F]{
  def find(email: String): F[Option[User]] =
    sql"SELECT * FROM users WHERE email=$email"
      .query[User]
      .option
      .transact(xa)

  def create(user: User): F[Either[String, String]] =
    find(user.email) flatMap{
      case None => sql"""
      INSERT INTO users(
      email,
      hashedPassword,
      firstName,
      lastName,
      company,
      role)
      VALUES(
      ${user.email},
      ${user.hashedPassword},
      ${user.firstName},
      ${user.lastName},
      ${user.company},
      ${user.role}
      );
      """.update.run.transact(xa).map(_ => Right(user.email))
      case Some(_) => Left("Email already exists").pure[F]
    }
    

  def update(user: User): F[Option[User]] =
    for {
      _ <- sql"""
       UPDATE users SET
        hashedPassword = ${user.hashedPassword},
        firstName = ${user.firstName},
        lastName = ${user.lastName},
        company = ${user.company},
        role = ${user.role}
       WHERE email = ${user.email}
         """.update.run.transact(xa)
      maybeUser <- find(user.email)
    } yield maybeUser

  def delete(email: String): F[Boolean] =
    sql"DELETE FROM users WHERE email=$email".update.run.transact(xa).map(_ == 1)
}

object LiveUsers {
  def apply[F[_]: MonadCancelThrow: Logger](xa: Transactor[F]): F[LiveUsers[F]] = new LiveUsers[F](xa).pure[F]
}
