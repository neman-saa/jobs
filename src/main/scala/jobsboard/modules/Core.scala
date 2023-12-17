package jobsboard.modules

import cats.effect.{Concurrent, Resource}
import jobsboard.core.*
import doobie.*
import doobie.implicits.*
import doobie.util.*
import doobie.hikari.HikariTransactor
import cats.effect.Async
import cats.implicits.*
import jobsboard.configuration.SecurityConfig
import jobsboard.core.{Jobs, LiveJobs}
import org.typelevel.log4cats.Logger

final class Core[F[_]](val jobs: Jobs[F])(val auth: Auth[F]) {
}

object Core {

  def apply[F[_]: Async: Logger](xa: Transactor[F])(securityConfig: SecurityConfig): Resource[F, Core[F]] = {

    val coreF = for {
      jobs <- LiveJobs[F](xa)
      users <- LiveUsers[F](xa)
      auth <- LiveAuth[F](users)(securityConfig)
    } yield new Core(jobs)(auth)

    Resource.eval(coreF)
  }
}
