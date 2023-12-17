package jobsboardTest.http.routes

import io.circe.generic.auto.*
import cats.effect.testing.scalatest.AsyncIOSpec
import jobsboardTest.fixtures.{JobFixture, SecuredRouteFixture}
import org.scalatest.freespec.AsyncFreeSpec
import cats.effect.*
import cats.implicits.*
import com.github.dockerjava.zerodep.shaded.org.apache.hc.client5.http.HttpRoute
import jobsboard.core.Jobs
import jobsboard.domain.{job, pagination}
import jobsboard.domain.job.{Job, JobFilter, JobInfo}
import jobsboard.domain.pagination.Pagination
import jobsboard.http.routes.JobRoutes
import org.http4s.HttpRoutes
import org.http4s.dsl.*
import org.http4s.implicits.*
import org.scalatest.matchers.should.Matchers
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.http4s.*
import org.http4s.circe.CirceEntityCodec.*

import java.util.UUID

class JobRouteSpec
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with Http4sDsl[IO]
    with JobFixture
    with SecuredRouteFixture {

  val jobs: Jobs[IO] = new Jobs[IO] {
    override def create(ownerEmail: String, jobsInfo: JobInfo): IO[UUID] = IO.pure(NewJobUuid)

    override def all(): IO[List[Job]] = IO.pure(List(AwesomeJob))

    override def all(filter: JobFilter, pagination: Pagination): IO[List[Job]] =
      IO.pure(List(AwesomeJob))
    override def find(id: UUID): IO[Option[Job]] = {
      if (id == AwesomeJob.id) IO.pure(Some(AwesomeJob))
      else IO.pure(none)
    }

    override def update(id: UUID, jobInfo: JobInfo): IO[Option[job.Job]] =
      if (id == AwesomeJob.id) Some(UpdatedAwesomeJob).pure[IO]
      else none.pure[IO]

    override def delete(id: UUID): IO[Int] =
      if (id == AwesomeJob.id) 0.pure[IO]
      else 1.pure[IO]
  }
  given logger: Logger[IO]     = Slf4jLogger.getLogger[IO]
  val jobRoute: HttpRoutes[IO] = JobRoutes[IO](jobs, mockedAuthenticator).allRoutes

  "Job routes" - {
    "Should return a job with a given id" in {
      for {
        response <- jobRoute.orNotFound.run(
          Request(method = Method.GET, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f40064")
        )
        retrieved <- response.as[Job]
      } yield {
        response.status shouldBe Status.Ok
        retrieved shouldBe AwesomeJob
      }
    }
    "Should return all jobs" in {
      for {
        response <- jobRoute.orNotFound.run(
          Request(method = Method.GET, uri = uri"/jobs")
            .withEntity(JobFilter(remote = true))
        )
        retrieved <- response.as[List[Job]]
      } yield {
        response.status shouldBe Status.Ok
        retrieved shouldBe List(AwesomeJob)
      }
    }

    "Should create a new job" in {
      for {
        jwtToken <- mockedAuthenticator.create(danielEmail)
        response <- jobRoute.orNotFound.run(
          Request[IO](method = Method.POST, uri = uri"/jobs/create")
            .withEntity(AwesomeJob.jobInfo)
            .withBearerToken(jwtToken)
        )
        retrieved <- response.as[UUID]
      } yield {
        response.status shouldBe Status.Created
        retrieved shouldBe NewJobUuid
      }
    }

    "Should only update a job with correct id" in {
      for {
        jwtToken <- mockedAuthenticator.create(danielEmail)
        responseOk <- jobRoute.orNotFound.run(
          Request[IO](method = Method.PUT, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f40064")
            .withEntity(UpdatedAwesomeJob.jobInfo)
            .withBearerToken(jwtToken)
        )
        responseInvalidNoToken <- jobRoute.orNotFound.run(
          Request(method = Method.PUT, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f40064")
            .withEntity(UpdatedAwesomeJob.jobInfo)
        )
        responseInvalidNotFound <- jobRoute.orNotFound.run(
          Request[IO](method = Method.PUT, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f40000")
            .withEntity(UpdatedAwesomeJob.jobInfo)
            .withBearerToken(jwtToken)
        )
      } yield {
        responseOk.status shouldBe Status.Ok
        responseInvalidNoToken.status shouldBe Status.Unauthorized
        responseInvalidNotFound.status shouldBe Status.NotFound
      }
    }

    "Should only delete a job that exists" in {
      for {
        jwtToken <- mockedAuthenticator.create(danielEmail)
        responseOk <- jobRoute.orNotFound.run(
          Request[IO](method = Method.DELETE, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f40064")
            .withBearerToken(jwtToken)
        )
        responseInvalid <- jobRoute.orNotFound.run(
          Request[IO](method = Method.DELETE, uri = uri"/jobs/843df718-ec6e-4d49-9289-f799c0f40000")
            .withEntity(UpdatedAwesomeJob.jobInfo)
            .withBearerToken(jwtToken)
        )
      } yield {
        responseOk.status shouldBe Status.Ok
        responseInvalid.status shouldBe Status.NotFound
      }
    }
  }
}
