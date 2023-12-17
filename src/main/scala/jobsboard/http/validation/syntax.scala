package jobsboard.http.validation

import io.circe.generic.auto.*
import org.http4s.circe.CirceEntityCodec.*
import cats.*
import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNel
import cats.effect.Concurrent
import cats.implicits.*
import jobsboard.http.responses.FailureResponse
import jobsboard.http.validation.validators.{ValidationFailure, Validator}
import org.http4s.*
import org.http4s.implicits.*
import org.http4s.dsl.*
import org.typelevel.log4cats.Logger
import jobsboard.logging.syntax.*

object syntax {

  private def validateEntity[A](entity: A)(using validator: Validator[A]): ValidatedNel[ValidationFailure, A] =
    validator.validate(entity)

  trait HttpValidationDsl[F[_]: Concurrent: Logger] extends Http4sDsl[F] {
    extension (req: Request[F])
      def validate[A: Validator](
          serverLogicIfValid: A => F[Response[F]]
      )(using EntityDecoder[F, A]): F[Response[F]] =
        req
          .as[A]
          .logError(e => s"Parsing payload failed: $e")
          .map(validateEntity)
          .flatMap {
            case Valid(entity) => serverLogicIfValid(entity)
            case Invalid(errors) =>
              BadRequest(
                FailureResponse(errors.toList.map(error => error.message).mkString("[", ",", "]"))
              )
          }
  }
}
