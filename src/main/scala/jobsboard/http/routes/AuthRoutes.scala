package jobsboard.http.routes

import io.circe.generic.auto.*
import org.http4s.circe.CirceEntityCodec.*
import cats.effect.Concurrent
import jobsboard.core.Auth
import jobsboard.http.validation.syntax.HttpValidationDsl
import org.http4s.{HttpRoutes, Response, Status}
import org.http4s.server.Router
import org.typelevel.log4cats.Logger
import cats.implicits.*
import jobsboard.domain.auth.{LoginInfo, NewPasswordInfo}
import jobsboard.domain.security.*
import jobsboard.domain.user.{NewUserInfo, User}
import jobsboard.http.responses.FailureResponse
import tsec.authentication.{SecuredRequestHandler, TSecAuthService, asAuthed}

class AuthRoutes[F[_]: Concurrent: Logger](auth: Auth[F]) extends HttpValidationDsl[F] {

  private val authenticator = auth.authenticator
  private val securedHandler: SecuredRequestHandler[F, String, User, JwtToken] =
    SecuredRequestHandler(authenticator)

  private val loginRoute: HttpRoutes[F] = HttpRoutes.of[F] { case req @ POST -> Root / "login" =>
    req.validate[LoginInfo] { loginInfo =>
      val mbTokenF = for {
        mbToken <- auth.login(loginInfo.email, loginInfo.password)
        _       <- Logger[F].info(s"User logging in: ${loginInfo.email}")
      } yield mbToken
      mbTokenF.map {
        case Some(token) => authenticator.embed(Response(Status.Ok), token)
        case None        => Response(Status.Unauthorized)
      }
    }
  }

  private val signUpRoute: HttpRoutes[F] = HttpRoutes.of[F] { case req@POST -> Root / "users" =>
    req.validate[NewUserInfo]{newUserInfo =>
      for {
        maybeNewUser <- auth.signUp(newUserInfo)
        response <- maybeNewUser match {
          case Some(user) => Created(user.email)
          case None => BadRequest("User with this email already exists")
        }
      } yield response
    }

  }

  private val passwordResetRoute: AuthRoute[F] = {
    case req@POST -> Root / "users" / "password" asAuthed user =>
      req.request.validate[NewPasswordInfo]{ newPasswordInfo =>
        for {
          maybeUserOrError <- auth.changePassword(user.email, newPasswordInfo)
          resp <- maybeUserOrError match {
            case Right(Some(_)) => Ok()
            case Right(None) => NotFound(FailureResponse(s"User email ${user.email} not found"))
            case Left(e) => Forbidden()
          }
        } yield resp
     }
  }

  private val logoutRoute: AuthRoute[F] = { 
    case req@POST -> Root / "logout" asAuthed _ =>
    val token = req.authenticator
    for {
      _    <- authenticator.discard(token)
      resp <- Ok()
    } yield resp
  }

  private val deleteUser: AuthRoute[F] = {
    case req@DELETE -> Root / "users" / email asAuthed user => {
      auth.delete(email).flatMap {
        case true => Ok()
        case false => NotFound()
      }
    }
  }

  private val unAuthedRoutes = loginRoute <+> signUpRoute
  private val authedRoutes =
    securedHandler.liftService(
      logoutRoute.restrictedTo(allRoles) |+|
      deleteUser.restrictedTo(adminOnly) |+|
      passwordResetRoute.restrictedTo(allRoles))
  val allRoutes: HttpRoutes[F] = Router(
    "/auth" -> (unAuthedRoutes <+> authedRoutes)
  )
}

object AuthRoutes {
  def apply[F[_]: Concurrent: Logger](auth: Auth[F]) = new AuthRoutes[F](auth)
}
