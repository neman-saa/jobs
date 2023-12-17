package jobsboardTest.core

import cats.effect.*
import cats.effect.testing.scalatest.AsyncIOSpec
import jobsboard.configuration.TokenConfig
import jobsboard.core.LiveTokens
import jobsboardTest.fixtures.UserFixture
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import scala.concurrent.duration.*
class TokensSpec
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with DoobieSpec
    with UserFixture {

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  override val initScript: String = "sql/recoverytokens.sql"

  "Tokens 'algebra'" - {
    "should not create a token for a non-existing user" in {
      val program = transactor.use { xa =>
        for {
          tokens <- LiveTokens[IO](mockedUsers)(xa, TokenConfig(10000000L))
          token  <- tokens.getToken("gkgmokgmok")
        } yield token
      }
      program.asserting(_ shouldBe None)
    }

    "should create a token for an existing user" in {
      val program = transactor.use { xa =>
        for {
          tokens <- LiveTokens[IO](mockedUsers)(xa, TokenConfig(10000000L))
          token  <- tokens.getToken(danielEmail)
        } yield token
      }
      program.asserting(_ shouldBe defined)
    }

    "should not validate expired tokens" in {
      val program = transactor.use { xa =>
        for {
          tokens <- LiveTokens[IO](mockedUsers)(xa, TokenConfig(100L))
          token  <- tokens.getToken(danielEmail)
          _      <- IO.sleep(1.second)
          isValidToken <- token match {
            case Some(token) => tokens.checkToken(danielEmail, token)
            case None        => IO(false)
          }
        } yield isValidToken
      }
      program.asserting(_ shouldBe false)
    }

    "should validate new tokens" in {
      val program = transactor.use { xa =>
        for {
          tokens <- LiveTokens[IO](mockedUsers)(xa, TokenConfig(1000000L))
          token  <- tokens.getToken(danielEmail)
          _      <- IO.sleep(1.second)
          isValidToken <- token match {
            case Some(token) => tokens.checkToken(danielEmail, token)
            case None        => IO(false)
          }
        } yield isValidToken
      }
      program.asserting(_ shouldBe true)
    }

    "should only validate tokens for the user it generated for" in {
      val program = transactor.use { xa =>
        for {
          tokens <- LiveTokens[IO](mockedUsers)(xa, TokenConfig(100L))
          token  <- tokens.getToken(danielEmail)
          isValidToken <- token match {
            case Some(token) => tokens.checkToken(danielEmail, token)
            case None        => IO(false)
          }
          isNotValidToken <- token match {
            case Some(token) => tokens.checkToken("gpokgpmgpi", token)
            case None        => IO(false)
          }
        } yield (isValidToken, isNotValidToken)
      }
      program.asserting(x => { x._1 shouldBe true; x._2 shouldBe false })
    }
  }
}
