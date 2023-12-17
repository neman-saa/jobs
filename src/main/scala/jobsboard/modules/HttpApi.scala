package jobsboard.modules

import cats.*
import cats.effect.{Concurrent, Resource}
import cats.implicits.*
import jobsboard.http.routes.{HealthRoutes, JobRoutes}
import jobsboard.http.routes.*
import org.http4s.*
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.server.*
import org.typelevel.log4cats.Logger
import jobsboard .modules.*
private class HttpApi[F[_]: Concurrent: Logger](core: Core[F]) {
  private val healthRoutes = HealthRoutes[F].routes
  private val jobsRoutes = JobRoutes[F](core.jobs, core.auth.authenticator).allRoutes
  private val authRoutes = AuthRoutes[F](core.auth).allRoutes
  val endpoints: HttpRoutes[F] = Router(
    "/api" -> (healthRoutes <+> jobsRoutes <+> authRoutes)
  )
}

object HttpApi {
  def apply[F[_]: Concurrent: Logger](core: Core[F]): Resource[F, HttpApi[F]] = Resource.pure(new HttpApi[F](core))
}
