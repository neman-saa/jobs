package jobsboardTest.core

import cats.effect.testing.scalatest.AsyncIOSpec
import jobsboard.core.{LiveJobs, LiveUsers}
import jobsboardTest.fixtures.JobFixture
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import cats.effect.*
import jobsboard.domain.job.JobFilter
import jobsboard.domain.pagination.Pagination
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import jobsboardTest.fixtures.UserFixture
import doobie.implicits.*
import org.postgresql.util.PSQLException
import org.scalatest.Inside.inside

class UsersSpec
  extends AsyncFreeSpec
  with AsyncIOSpec
  with Matchers
  with DoobieSpec
  with UserFixture{
  override val initScript: String = "sql/users.sql"

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  "Users 'algebra'" - {
    "Should retrieve a user by email" in {
      transactor.use { xa =>
        val program = for {
          users <- LiveUsers[IO](xa)
          retrieved <- users.find("riccardo@rockthejvm.com")
        } yield retrieved
        program.asserting(_ shouldBe Some(Riccardo))
      }
    }
    "Should return None id email does not exist" in {
      transactor.use { xa =>
        val program = for {
          users <- LiveUsers[IO](xa)
          retrieved <- users.find("notFound@gmail.com")
        } yield retrieved
        program.asserting(_ shouldBe None)
      }
    }
    "Should create a new user" in {
      transactor.use { xa =>
        val program = for {
          users <- LiveUsers[IO](xa)
          retrieved <- users.create(NewUser)
        } yield retrieved
        program.asserting(_ shouldBe Right(NewUser.email))
      }
    }

    "Should fail creating a new user if email exists" in {
      transactor.use { xa =>
        val program = for {
          users <- LiveUsers[IO](xa)
          retrieved <- users.create(Daniel)
        } yield retrieved
        program.asserting({
          case Left(e) => e shouldBe "Email already exists"
        })
      }
    }

    "Should return none if updating a user that does not exist" in {
      transactor.use { xa =>
        val program = for {
          users <- LiveUsers[IO](xa)
          retrieved <- users.update(NewUser)
        } yield retrieved
        program.asserting(_ shouldBe None)
      }
    }

    "Should update existing user" in {
      transactor.use { xa =>
        val program = for {
          users <- LiveUsers[IO](xa)
          retrieved <- users.update(UpdatedRiccardo)
        } yield retrieved
        program.asserting(_ shouldBe Some(UpdatedRiccardo))
      }
    }

    "Should delete a user" in {
      transactor.use { xa =>
        val program = for {
          users <- LiveUsers[IO](xa)
          retrieved <- users.delete("daniel@rockthejvm.com")
        } yield retrieved
        program.asserting(_ shouldBe true)
      }
    }

    "Should not delete a user if does not exist" in {
      transactor.use { xa =>
        val program = for {
          users <- LiveUsers[IO](xa)
          retrieved <- users.delete("kkm")
        } yield retrieved
        program.asserting(_ shouldBe false)
      }
    }
  }
}
