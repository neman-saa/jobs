package jobsboard.playground

import cats.effect.{IO, IOApp}
import tsec.passwordhashers.PasswordHash
import tsec.passwordhashers.jca.BCrypt

object PasswordHashingPlayground extends IOApp.Simple {

  override def run: IO[Unit] = BCrypt.hashpw[IO]("newpw").map(println) *>
    BCrypt
      .checkpwBool[IO](
        "Million100000",
        PasswordHash[BCrypt]("$2a$10$bkCsbWAfaBPiSt4mo.nFv.2xMJQzfBofDuVdsKFY/559H8s777PYC")
      )
      .map(println)
}
