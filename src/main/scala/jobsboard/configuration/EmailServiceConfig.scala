package jobsboard.configuration

import pureconfig.*
import pureconfig.generic.derivation.default.*
case class git comigit EmailServiceConfig(
    host: String,
    port: Int,
    user: String,
    pass: String,
    frontendUrl: String) derives ConfigReader
