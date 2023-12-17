package jobsboardTest.fixtures

import cats.data.OptionT
import cats.effect.IO
import jobsboard.domain.security.{Authenticator, JwtToken}
import jobsboard.domain.user.User
import tsec.authentication.{IdentityStore, JWTAuthenticator}
import tsec.mac.jca.HMACSHA256

import scala.concurrent.duration.*
import jobsboardTest.fixtures.*
import org.http4s.headers.Authorization
import org.http4s.{AuthScheme, Credentials, Request}
import tsec.jws.mac.JWTMac

trait SecuredRouteFixture extends UserFixture {

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

  extension (request: Request[IO])
    def withBearerToken(a: JwtToken): Request[IO] = request.putHeaders {
      val jwtString = JWTMac.toEncodedString[IO, HMACSHA256](a.jwt)
      Authorization(Credentials.Token(AuthScheme.Bearer, jwtString))
    }
}
