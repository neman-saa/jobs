package jobsboard.http.routes

import cats.effect.Concurrent
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router

private class HealthRoutes[F[_]: Concurrent] extends Http4sDsl[F] {
  private val healthEndpoint: HttpRoutes[F] = HttpRoutes.of[F] { case GET -> Root =>
    Ok("All going great")
  }
  val routes: HttpRoutes[F] = Router(
    "/health" -> healthEndpoint
  )
}

object HealthRoutes {
  def apply[F[_]: Concurrent] = new HealthRoutes[F]
}
