package jobsboard.http.routes

import io.circe.generic.auto.*
import org.http4s.circe.CirceEntityCodec.*
import cats.effect.{Concurrent, Ref}
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import cats.*
import cats.implicits.*
import jobsboard.core.Jobs
import jobsboard.domain.job.JobInfo
import jobsboard.http.responses.FailureResponse
import jobsboard.domain.job.*
import jobsboard.domain.pagination.Pagination
import jobsboard.domain.security.{AuthRoute, Authenticator, JwtToken}
import jobsboard.domain.user.User
import org.typelevel.log4cats.Logger
import jobsboard.http.validation.syntax.*
import jobsboard.domain.security.*
import java.util.UUID
import scala.collection.mutable
import jobsboard.logging.syntax.*
import tsec.authentication.{SecuredRequestHandler, asAuthed}

private class JobRoutes[F[_] : Concurrent : Logger](jobs: Jobs[F])(authenticator: Authenticator[F]) extends HttpValidationDsl[F] {


  private val securedHandler: SecuredRequestHandler[F, String, User, JwtToken] =
    SecuredRequestHandler(authenticator)

  private object OffsetQueryParam extends OptionalQueryParamDecoderMatcher[Int]("offset")
  private object LimitQueryParam extends OptionalQueryParamDecoderMatcher[Int]("limit")

  private val allJobsRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case req@GET -> Root :? LimitQueryParam(limit) +& OffsetQueryParam(offset) => for {
      filter <- req.as[JobFilter]
      jobsList <- jobs.all(filter, Pagination.tr(limit, offset))
      resp <- Ok(jobsList)
    } yield resp
  }

  private val findJobRoute: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / UUIDVar(id) =>
      jobs.find(id) flatMap {
        case Some(job) => Ok(job)
        case None => NotFound(FailureResponse(s"No job with id: $id found"))
      }
  }

  private val createJobRoute: AuthRoute[F] = {
    case req@POST -> Root / "create" asAuthed _ => req.request.validate[JobInfo] { jobInfo =>
      for {
        _ <- Logger[F].info(s"Parsed job info: $jobInfo")
        job <- jobs.create("TODO", jobInfo)
        - <- Logger[F].info(s"Created a job: ${job}")
        response <- Created(job)
      } yield response
    }
  }

  private val updateJobRoute: AuthRoute[F] = {
    case req@PUT -> Root / UUIDVar(id) asAuthed user => req.request.validate[JobInfo] { jobInfo =>

      jobs.find(id).flatMap {
        case None => NotFound(FailureResponse(s"Cannot delete a job: $id: not found."))
        case Some(value) if value.ownerEmail == user.email || user.isAdmin =>
          jobs.update(id, jobInfo) *> Ok("Updated")
        case _ => Forbidden(FailureResponse("You can only update your jobs"))
      }
    }
  }

  private val deleteJobRoute: AuthRoute[F] = {
    case DELETE -> Root / UUIDVar(id) asAuthed user =>
      jobs.find(id) flatMap {
        case Some(job) if user.owns(job) || user.isAdmin => for {
          _ <- jobs.delete(id)
          resp <- Ok()
        } yield resp
        case None => NotFound(FailureResponse(s"Cannot delete, job with id: $id not found"))
        case _ => Forbidden(FailureResponse("You can only delete your jobs"))
      }
  }

  val authRoutes = securedHandler.liftService(
    createJobRoute.restrictedTo(allRoles) |+|
    updateJobRoute.restrictedTo(allRoles) |+|
    deleteJobRoute.restrictedTo(allRoles)
  )
  val unauthedRoutes: HttpRoutes[F] = allJobsRoute <+> findJobRoute
  

  val allRoutes = Router(
    "/jobs" -> (unauthedRoutes <+> authRoutes)
  )
}

object JobRoutes {
  def apply[F[_] : Concurrent : Logger](jobs: Jobs[F], authenticator: Authenticator[F]) = new JobRoutes[F](jobs)(authenticator)
}
