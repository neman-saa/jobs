package jobsboard.modules

import cats.effect.{Concurrent, Resource}
import jobsboard.core.*
import doobie.*
import doobie.implicits.*
import doobie.util.*
import doobie.hikari.HikariTransactor
import cats.effect.Async
import cats.implicits.*
import jobsboard.configuration.{EmailServiceConfig, SecurityConfig, TokenConfig}
import jobsboard.core.{Jobs, LiveJobs}
import org.typelevel.log4cats.Logger

final class Core[F[_]](val jobs: Jobs[F])(val auth: Auth[F]) {}

object Core {

  def apply[F[_]: Async: Logger](
      xa: Transactor[F]
  )(securityConfig: SecurityConfig, tokenConfig: TokenConfig, emailServiceConfig: EmailServiceConfig): Resource[F, Core[F]] = {

    val coreF = for {
      jobs  <- LiveJobs[F](xa)
      users <- LiveUsers[F](xa)
      tokens <- LiveTokens[F](users)(xa, tokenConfig)
      emails <- LiveEmails[F](emailServiceConfig)
      auth  <- LiveAuth[F](users, tokens, emails)(securityConfig)
    } yield new Core(jobs)(auth)

    Resource.eval(coreF)
  }
}
