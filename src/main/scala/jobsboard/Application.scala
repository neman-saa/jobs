package jobsboard

import cats.effect.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.*
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import cats.*
import cats.effect.IO
import cats.effect.IOApp
import jobsboard.configuration.{AppConfig, EmberConfig}
import jobsboard.modules.{Core, Database, HttpApi}
import pureconfig.ConfigReader.Result
import pureconfig.error.ConfigReaderException
import pureconfig.{ConfigReader, ConfigSource}
import jobsboard.configuration.*
import jobsboard.configuration.syntax.*
import jobsboard.modules.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object Application extends IOApp.Simple {
  val configSource: Result[EmberConfig] = ConfigSource.default.load[EmberConfig]

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]
  override def run: IO[Unit] = ConfigSource.default.loadF[IO, AppConfig].flatMap {
    case AppConfig(emberConfig, postgresConfig, securityConfig) => {
      val appResource = for {
        xa <- Database.postgresResource[IO](postgresConfig)
        core <- Core[IO](xa)(securityConfig)
        httpApi <- HttpApi[IO](core)
        server <- EmberServerBuilder
          .default[IO]
          .withHost(emberConfig.host)
          .withPort(emberConfig.port)
          .withHttpApp(httpApi.endpoints.orNotFound)
          .build
      } yield server
      appResource.use(_ => IO.println("Server started") *> IO.never)
    }
  }
}
