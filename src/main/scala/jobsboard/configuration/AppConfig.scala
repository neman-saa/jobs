package jobsboard.configuration

import pureconfig.ConfigReader
import pureconfig.generic.derivation.default.*

case class AppConfig(
    emberConfig: EmberConfig,
    postgresConfig: PostgresConfig,
    securityConfig: SecurityConfig,
    tokenConfig: TokenConfig,
    emailServiceConfig: EmailServiceConfig
) derives ConfigReader
