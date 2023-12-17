package jobsboard.domain

import cats.{Applicative, Monad, MonadThrow, Semigroup}
import jobsboard.domain.user.{Role, User}
import org.http4s.{Response, Status}
import tsec.authentication.{AugmentedJWT, JWTAuthenticator, SecuredRequest, TSecAuthService}
import tsec.authorization.{AuthorizationInfo, BasicRBAC}
import tsec.mac.jca.HMACSHA256
import cats.implicits.*

object security {
  type JwtToken            = AugmentedJWT[HMACSHA256, String]
  type Authenticator[F[_]] = JWTAuthenticator[F, String, User, HMACSHA256]
  type AuthRoute[F[_]]     = PartialFunction[SecuredRequest[F, User, JwtToken], F[Response[F]]]
  type AuthRBAC[F[_]]      = BasicRBAC[F, Role, User, JwtToken]

  given authRole[F[_]: MonadThrow]: AuthorizationInfo[F, Role, User] with {
    override def fetchInfo(u: User): F[Role] = u.role.pure[F]
  }
  def allRoles[F[_]: MonadThrow]: AuthRBAC[F] =
    BasicRBAC.all[F, Role, User, JwtToken]

  def adminOnly[F[_]: MonadThrow]: AuthRBAC[F] = BasicRBAC(Role.ADMIN)

  def recruiterOnly[F[_]: MonadThrow]: AuthRBAC[F] = BasicRBAC(Role.RECRUITER)

  case class Authorizations[F[_]](rbacRoutes: Map[AuthRBAC[F], List[AuthRoute[F]]])

  object Authorizations {
    given combiner[F[_]]: Semigroup[Authorizations[F]] = Semigroup.instance(
      (authA, authB) => Authorizations(authA.rbacRoutes |+| authB.rbacRoutes))
  }

  extension [F[_]](authRoute: AuthRoute[F])
    def restrictedTo(rbac: AuthRBAC[F]): Authorizations[F] =
      Authorizations(Map(rbac -> List(authRoute)))

  given authToTSec[F[_]: Monad]: Conversion[Authorizations[F], TSecAuthService[User, JwtToken, F]] =
    authz => {
      val unAuthorizedService: TSecAuthService[User, JwtToken, F] =
        TSecAuthService[User, JwtToken, F] { _ =>
          Response[F](Status.Unauthorized).pure[F]
        }
      //val rbac: AuthRBAC[F]       = ???
      //val authRoute: AuthRoute[F] = ???
      //val tsec = TSecAuthService.withAuthorizationHandler(rbac)(authRoute, unAuthorizedService.run)

      authz.rbacRoutes.toSeq.foldLeft(unAuthorizedService) { case (acc, (rbac, routes)) =>
        val bigRoute = routes.reduce(_.orElse(_))
        TSecAuthService.withAuthorizationHandler(rbac)(bigRoute, acc.run)
      }
    }
}
