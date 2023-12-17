package jobsboard.configuration

import pureconfig.*
import pureconfig.generic.derivation.default.*
case class EmailServiceConfig(
    host: String,
    port: Int,
    user: String,
    pass: String,
    frontendUrl: String) derives ConfigReader
