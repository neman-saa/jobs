package jobsboardTest.core

import cats.effect.*
import cats.data.OptionT
import cats.implicits.*
import cats.effect.testing.scalatest.AsyncIOSpec
import jobsboard.configuration.SecurityConfig
import jobsboard.core.{Emails, LiveAuth, Tokens, Users}
import jobsboard.domain.auth.NewPasswordInfo
import jobsboard.domain.security.Authenticator
import jobsboard.domain.user.{NewUserInfo, User}
import jobsboardTest.fixtures.UserFixture
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import tsec.authentication.{IdentityStore, JWTAuthenticator}
import tsec.mac.jca.HMACSHA256
import tsec.passwordhashers.{PasswordHash, jca}
import tsec.passwordhashers.jca.BCrypt

import scala.concurrent.duration.*

class AuthSpec extends AsyncFreeSpec with AsyncIOSpec with Matchers with UserFixture {

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  /*
  key for hashing
  identity store to retrieve users
  jwt authenticator based on key and store
   */
  val mockedAuthenticator: Authenticator[IO] = {
    val key = HMACSHA256.unsafeGenerateKey
    val idStore: IdentityStore[IO, String, User] =
      (email: String) =>
        if (email == danielEmail) OptionT.pure[IO](Daniel)
        else if (email == riccardoEmail) OptionT.pure[IO](Riccardo)
        else OptionT.none[IO, User]
    JWTAuthenticator.unbacked.inBearerToken(
      1.day,
      None,
      idStore,
      key
    )
  }

  val mockedTokens: Tokens[IO] = new Tokens[IO] {
    override def getToken(email: String): IO[Option[String]] =
      if (email == danielEmail) IO.pure(Some("abc123"))
      else IO.pure(None)

    override def checkToken(email: String, token: String): IO[Boolean] =
      IO.pure(token == "abc123")
  }

  val mockedEmails: Emails[IO] = new Emails[IO] {
    override def sendPasswordRecoveryEmail(to: String, token: String): IO[Unit] = IO.unit

    override def sendEmail(to: String, subject: String, content: String): IO[Unit] = IO.unit
  }

  val mockedConfig: SecurityConfig = SecurityConfig("secret", 1.day)
  "Auth 'algebra'" - {
    "should return none if user does not exist" in {
      val program = for {
        auth       <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)(mockedConfig)
        maybeToken <- auth.login("user@gmail.com", "password")
      } yield maybeToken
      program.asserting(_ shouldBe None)
    }

    "should return none if user exists but password invalid" in {
      val program = for {
        auth       <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)(mockedConfig)
        maybeToken <- auth.login("daniel@rockthejvm.com", "passwordddddd")
      } yield maybeToken
      program.asserting(_ shouldBe None)
    }

    "should return a token if user exists" in {
      val program = for {
        auth       <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)(mockedConfig)
        maybeToken <- auth.login("daniel@rockthejvm.com", danielPassword)
      } yield maybeToken
      program.asserting(_ shouldBe defined)
    }

    "sign up should not create a new user if it already exists" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)(mockedConfig)
        maybeUser <- auth.signUp(
          NewUserInfo(danielEmail, "somePassword", Some("Daniel"), None, None)
        )
      } yield maybeUser
      program.asserting(_ shouldBe None)
    }

    "sign up should create a new user if does not exist" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)(mockedConfig)
        maybeToken <- auth.signUp(
          NewUserInfo("bob@rockthejvm.com", "somePassword", Some("Bob"), None, None)
        )
        isPasswordCorrect <- maybeToken.traverse(x =>
          BCrypt.checkpwBool[IO]("somePassword", PasswordHash[BCrypt](x.hashedPassword))
        )
      } yield isPasswordCorrect
      program.asserting {
        case Some(bool) => bool shouldBe true
        case x @ None   => x shouldBe None
      }
    }

    "should return Right(None) when changing password of unexisting user" in {
      val program = for {
        auth  <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)(mockedConfig)
        maybe <- auth.changePassword("alice@rockthejvm.com", NewPasswordInfo("oldpw", "newpw"))
      } yield maybe
      program.asserting(_ shouldBe Right(None))
    }

    "should return Right(Some) when changing password of existing user old password correct" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)(mockedConfig)
        maybe <- auth.changePassword(
          "daniel@rockthejvm.com",
          NewPasswordInfo(danielPassword, "newpw")
        )
        isNicePassword <- maybe match {
          case Right(Some(value)) =>
            BCrypt.checkpwBool[IO]("newpw", PasswordHash[BCrypt](value.hashedPassword))
          case _ => IO.pure(false)
        }
      } yield isNicePassword

      program.asserting(_ shouldBe true)
    }

    "should return Left(String) when changing password of existing user but old password is incorrect" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)(mockedConfig)
        maybe <- auth.changePassword(
          "daniel@rockthejvm.com",
          NewPasswordInfo("wrongPassword", "newpw")
        )
      } yield maybe
      program.asserting(_ shouldBe Left("Current password is invalid"))
    }

    "recoverPassword should fail for a user that does not exist" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)(mockedConfig)
        result1 <- auth.recoverPasswordFromToken("someone@rockthejvm.com", "abc123", "igotya")
        result2 <- auth.recoverPasswordFromToken("someone@rockthejvm.com", "wrongtoken", "igotya")
      } yield (result1, result2)
      program.asserting(_ shouldBe (false, false))
    }

    "recoverPassword should fail for a user that exists, but token is incorrect" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)(mockedConfig)
        result <- auth.recoverPasswordFromToken("daniel@rockthejvm.com", "wrongtoken", "h4ck3d")
      } yield result
      program.asserting(_ shouldBe false)
    }

    "recoverPassword should not fail for a user that exists, and token is correct" in {
      val program = for {
        auth <- LiveAuth[IO](mockedUsers, mockedTokens, mockedEmails)(mockedConfig)
        result <- auth.recoverPasswordFromToken("daniel@rockthejvm.com", "abc123", "h4ck3d")
      } yield result
      program.asserting(_ shouldBe true)
    }
  }
}
