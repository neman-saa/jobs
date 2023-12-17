package jobsboardTest.http.routes

import cats.data.OptionT
import io.circe.generic.auto.*
import org.http4s.circe.CirceEntityCodec.*
import cats.effect.testing.scalatest.AsyncIOSpec
import org.http4s.dsl.Http4sDsl
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import cats.effect.*
import jobsboard.core.Auth
import jobsboard.domain.auth.{LoginInfo, NewPasswordInfo}
import jobsboard.domain.security.{Authenticator, JwtToken}
import jobsboard.domain.user.{NewUserInfo, Role, User}
import jobsboard.domain.{auth, user}
import jobsboard.http.routes.AuthRoutes
import jobsboardTest.fixtures.{SecuredRouteFixture, UserFixture}
import org.http4s.headers.Authorization
import org.http4s.{AuthScheme, Credentials, HttpRoutes, Method, Request, Status}
import org.http4s.implicits.uri
import org.typelevel.ci.{CIString, CIStringSyntax}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import tsec.authentication.{IdentityStore, JWTAuthenticator}
import tsec.jws.mac.JWTMac
import tsec.mac.jca.HMACSHA256
import tsec.passwordhashers.jca.BCrypt

import scala.concurrent.duration.*

class AuthRouteSpec
  extends AsyncFreeSpec
  with AsyncIOSpec
  with Matchers
  with Http4sDsl[IO]
  with UserFixture 
  with SecuredRouteFixture {

  val mockedAuth: Auth[IO] = new Auth[IO] {
    override def login(email: String, password: String): IO[Option[JwtToken]] =
      if (email == danielEmail && password == danielPassword)
        {mockedAuthenticator.create(danielEmail).map(Some.apply)}
      else IO.pure(None)

    override def signUp(newUserInfo: NewUserInfo): IO[Option[User]] =
      if(newUserInfo.email == danielEmail)
        IO.pure(None)
      else for {
        hashedpw <- BCrypt.hashpw[IO](newUserInfo.password)
      } yield
        Some(User(
          newUserInfo.email,
          hashedpw,
          newUserInfo.firstName,
          newUserInfo.secondName,
          newUserInfo.company,
          Role.RECRUITER
        ))

    override def changePassword(email: String, newPasswordInfo: auth.NewPasswordInfo): IO[Either[String, Option[user.User]]] =
      if (email == danielEmail)
        if (newPasswordInfo.oldPassword == danielPassword) IO.pure(Right(Some(Daniel)))
        else IO.pure(Left("Invalid password"))
      else IO.pure(Right(None))

    override def authenticator: Authenticator[IO] = mockedAuthenticator

    override def delete(email: String): IO[Boolean] =
      if (email == danielEmail) IO.pure(true)
      else IO.pure(false)
  }

  given logger: Logger[IO] = Slf4jLogger.getLogger

  val allRoutes: HttpRoutes[IO] = AuthRoutes[IO](mockedAuth).allRoutes

  "Auth routes" - {
    "should not give token if login fails, and vise versa" in {
      for {
        badResponse <- allRoutes.orNotFound.run(Request(
          method = Method.POST, uri = uri"auth/login")
          .withEntity(LoginInfo(danielEmail, "wrongPassword")))
      } yield {
        badResponse.status shouldBe Status.Unauthorized
      }
    }

    "should give token if login data is correct" in {
      for {
        goodResponse <- allRoutes.orNotFound.run(Request(
          method = Method.POST, uri = uri"auth/login")/////////////////////////////////////////////////////////////////////////////
          .withEntity(LoginInfo(danielEmail, danielPassword)))
      } yield {
        goodResponse.status shouldBe Status.Ok
      }
    }

    "should return Status.BadRequest if users email exists" in {
      for {
        badResponse <- allRoutes.orNotFound.run(Request(
          method = Method.POST, uri = uri"auth/users")/////////////////////////////////////////////////////////////////////////////
          .withEntity(NewUserInfo(Daniel.email, danielPassword, None, None, None)))
      } yield {
        badResponse.status shouldBe Status.BadRequest
      }
    }

    " should give Status.Ok if users email does not exist" in {
      for {
        goodResponse <- allRoutes.orNotFound.run(Request(
          method = Method.POST, uri = uri"auth/users")
          .withEntity(NewUserInfo("newuser@gmail.com", "newuserpassword", None, None, None))) ////////////////////////////////////////////////////////////
      } yield {
        goodResponse.status shouldBe Status.Created
      }
    }

    "should give Status.Ok if jwt token is correct" in {
      for {
        jwtToken <- mockedAuthenticator.create(danielEmail)/////////////////////////////////////////////////////////////////
        goodResponse <- allRoutes.orNotFound.run(Request(
          method = Method.POST, uri = uri"auth/logout")
          .withBearerToken(jwtToken))
      } yield {
        goodResponse.status shouldBe Status.Ok
      }
    }

    "should give Status.unathorized if no jwt token" in {
      for {
        badResponse <- allRoutes.orNotFound.run(Request(
          method = Method.POST, uri = uri"auth/logout"))
      } yield {
        badResponse.status shouldBe Status.Unauthorized
      }
    }

    "should reset Status.Unauthorized" in {
      for {
        badResponseIfNoJwtToken <- allRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"auth/users/password")
            .withEntity(NewPasswordInfo("oldpw", "newpw")))
      } yield {
        badResponseIfNoJwtToken.status shouldBe Status.Unauthorized
      }
    }

    "should return Status.Ok if jwt token and password is correct" in {
      for {
        jwtTokenGood <- mockedAuthenticator.create(danielEmail)
        goodResponse <- allRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"auth/users/password")
            .withBearerToken(jwtTokenGood)
            .withEntity(NewPasswordInfo(danielPassword, "newpwnewwwwpww")))
      } yield {
        goodResponse.status shouldBe Status.Ok
      }
    }

    "should return Status.unathorized if token is bad" in {
      for {
        jwtTokenBad <- mockedAuthenticator.create("not a good email")
        badResponseUserDoesNotExist <- allRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"auth/users/password")
            .withBearerToken(jwtTokenBad)
            .withEntity(NewPasswordInfo("oldpw", "newpw")))
      } yield {
        badResponseUserDoesNotExist.status shouldBe Status.Unauthorized
      }
    }

    "should return Status.Forbidden if password is incorrect" in {
      for {
        jwtTokenGood <- mockedAuthenticator.create(danielEmail)
        badResponseOldPasswordIncorrect <- allRoutes.orNotFound.run(
          Request(method = Method.POST, uri = uri"auth/users/password")
            .withBearerToken(jwtTokenGood)
            .withEntity(NewPasswordInfo("badOldPassword", "newpwnewpwnewpw")))
      } yield {
        badResponseOldPasswordIncorrect.status shouldBe Status.Forbidden
      }
    }
    "should return Status.Unauthorized if no jwt token" in {
      for {
        badResponse <- allRoutes.orNotFound.run(
          Request(method = Method.DELETE, uri = uri"auth/users/daniel@rockthejvm.com"))
      } yield {
        badResponse.status shouldBe Status.Unauthorized
      }
    }
    "should return Status.Ok if jwt token is correct" in {
      for {
        jwtToken <- mockedAuthenticator.create(danielEmail)
        badResponse <- allRoutes.orNotFound.run(
          Request(method = Method.DELETE, uri = uri"auth/users/daniel@rockthejvm.com")
            .withBearerToken(jwtToken))

      } yield {
        badResponse.status shouldBe Status.Ok
      }
    }
  }
}
