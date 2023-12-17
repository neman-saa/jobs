package jobsboard.core

import cats.data.OptionT
import cats.effect.IO
import cats.effect.kernel.{Async, MonadCancelThrow, Ref}
import doobie.util.transactor.Transactor
import jobsboard.domain.auth.NewPasswordInfo
import jobsboard.domain.security.{Authenticator, JwtToken}
import jobsboard.domain.user.{NewUserInfo, Role, User}
import org.typelevel.log4cats.Logger
import tsec.authentication.{AugmentedJWT, BackingStore, IdentityStore, JWTAuthenticator}
import tsec.mac.jca.HMACSHA256
import cats.implicits.*
import jobsboard.configuration.SecurityConfig
import tsec.common.SecureRandomId
import tsec.passwordhashers.PasswordHash
import tsec.passwordhashers.jca.BCrypt

import scala.concurrent.duration.*

trait Auth[F[_]] {
  def login(email: String, password: String): F[Option[JwtToken]]
  def signUp(newUserInfo: NewUserInfo): F[Option[User]]
  def changePassword(email: String, newPasswordInfo: NewPasswordInfo): F[Either[String, Option[User]]]
  def authenticator: Authenticator[F]
  def delete(email: String): F[Boolean]
  def sendPasswordRecoveryToken(email: String): F[Unit]
  def recoverPasswordFromToken(email: String, token: String, newPassword: String): F[Boolean]
}

class LiveAuth[F[_]: Async: Logger](users: Users[F], authenticatorr: Authenticator[F], tokens: Tokens[F], emails: Emails[F]) extends Auth[F] {
  override def authenticator: Authenticator[F] = authenticatorr
  override def login(email: String, password: String): F[Option[JwtToken]] = for {
    maybeUser <- users.find(email)
    mbValidatedUser <- maybeUser.filterA(user => BCrypt.checkpwBool[F](password, PasswordHash[BCrypt](user.hashedPassword)))
    mbJwtToken <- mbValidatedUser.traverse(user => authenticatorr.create(user.email))
  } yield mbJwtToken


  override def signUp(newUserInfo: NewUserInfo): F[Option[User]] =
    val signUpF = for {
      hashedPassword <- BCrypt.hashpw[F](newUserInfo.password)
      creation <- users.create(User(newUserInfo.email, hashedPassword, newUserInfo.firstName, newUserInfo.secondName, newUserInfo.company, Role.RECRUITER))
    } yield creation

    signUpF flatMap {
      case Left(_) => None.pure[F]
      case Right(email) => users.find(email)
    }

  override def changePassword(email: String, newPasswordInfo: NewPasswordInfo): F[Either[String, Option[User]]] =
    users.find(email) flatMap {
      case None => Right(None).pure[F]
      case Some(user) => for {
        isCorrectOld <- BCrypt.checkpwBool[F](newPasswordInfo.oldPassword, PasswordHash[BCrypt](user.hashedPassword))
        result <-
          if (isCorrectOld) for {
          hashedPasswordd <- BCrypt.hashpw[F](newPasswordInfo.newPassword)
          updatedUser <- users.update(user.copy(hashedPassword = hashedPasswordd))
          } yield Right(updatedUser)
          else Left("Current password is invalid").pure[F]
      } yield result
    }
  override def delete(email: String): F[Boolean] = users.delete(email)

  override def sendPasswordRecoveryToken(email: String): F[Unit] =
    tokens.getToken(email).flatMap {
      case Some(token) => emails.sendPasswordRecoveryEmail(email, token)
      case None => ().pure[F]
    }

  override def recoverPasswordFromToken(email: String, token: String, newPassword: String): F[Boolean] = for {
    maybeUser <- users.find(email)
    tokenIsValid <- tokens.checkToken(email, token)
    result <- (maybeUser, tokenIsValid) match {
      case (Some(user), true) => updateUser(user, newPassword).map(_.nonEmpty)
      case _ => false.pure[F]
    }
  } yield result

  private def updateUser(user: User, newPassword: String): F[Option[User]] = for {
    hashedPassword <- BCrypt.hashpw[F](newPassword)
    updateUser <- users.update(user.copy(hashedPassword = hashedPassword))
  } yield updateUser
}


object LiveAuth {
  def apply[F[_]: Async: Logger](users: Users[F], tokens: Tokens[F], emails: Emails[F])(securityConfig: SecurityConfig): F[LiveAuth[F]] = {
    val keyF = HMACSHA256.buildKey[F](securityConfig.secret.getBytes("UTF-8"))
    val idStore: IdentityStore[F, String, User] =
      (email: String) => OptionT(users.find(email))
    val tokenStoreF = Ref.of[F, Map[SecureRandomId, JwtToken]](Map.empty).map { ref =>
        new BackingStore[F, SecureRandomId, JwtToken] {
          override def get(id: SecureRandomId): OptionT[F, JwtToken] = OptionT(ref.get.map(_.get(id)))
          override def put(elem: JwtToken): F[JwtToken] = ref.modify(store => (store + (elem.id -> elem), elem))
          override def update(v: JwtToken): F[JwtToken] = put(v)
          override def delete(id: SecureRandomId): F[Unit] = ref.modify(store => (store - id, ()))
        }
      }
    val authenticatorF: F[Authenticator[F]] = for {
      key <- keyF
      tokenStore <- tokenStoreF
    } yield JWTAuthenticator.backed.inBearerToken(
      expiryDuration = securityConfig.jwtExpiryDuration,
      maxIdle = None,
      identityStore = idStore,
      tokenStore = tokenStore,
      signingKey = key)

    authenticatorF.map(authenticator => new LiveAuth[F](users, authenticator, tokens, emails))
  }
}

