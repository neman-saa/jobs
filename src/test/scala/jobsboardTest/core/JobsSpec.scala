package jobsboardTest.core

import cats.effect.testing.scalatest.AsyncIOSpec
import jobsboard.core.LiveJobs
import jobsboardTest.fixtures.JobFixture
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers
import cats.effect.*
import jobsboard.domain.job.JobFilter
import jobsboard.domain.pagination.Pagination
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
class JobsSpec
    extends AsyncFreeSpec
    with AsyncIOSpec
    with Matchers
    with JobFixture
    with DoobieSpec {
  override val initScript: String = "sql/jobs.sql"
  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  "Jobs algebra" - {
    "should only return a job if it exists" in {
      transactor.use { xa =>
        val program = for {
          jobs         <- LiveJobs[IO](xa)
          retrievedBad <- jobs.find(NotFoundJobUuid)
          retrievedOk  <- jobs.find(AwesomeJobUuid)
        } yield (retrievedBad, retrievedOk)
        program.asserting((x1, x2) => (x1, x2) shouldBe (None, Some(AwesomeJob)))
      }
    }
    "should retrieve all jobs" in {
      transactor.use { xa =>
        {
          val program = for {
            jobs      <- LiveJobs[IO](xa)
            retrieved <- jobs.all()
          } yield retrieved

          program.asserting(_ shouldBe List(AwesomeJob))
        }
      }
    }
    "should update a job by id" in {
      transactor.use(xa => {
        val program = for {
          jobs      <- LiveJobs[IO](xa)
          retrieved <- jobs.update(AwesomeJobUuid, UpdatedAwesomeJob.jobInfo)
        } yield retrieved
        program.asserting(_.map(_.jobInfo) shouldBe Some(UpdatedAwesomeJob.jobInfo))
      })
    }
    "should delete job by id" in {
      transactor.use(xa => {
        val program = for {
          jobs <- LiveJobs[IO](xa)
          retrievedDeleting <- jobs.delete(AwesomeJobUuid)
          retrievedFinding <- jobs.find(AwesomeJobUuid)
        } yield (retrievedDeleting, retrievedFinding)

        program.asserting((del, find) => (del, find) shouldBe (1, None))
      })
    }
    "should create a job" in {
      transactor.use (xa => {
        val program = for {
          jobs <- LiveJobs[IO](xa)
          retrievedCreate <- jobs.create("No email", AnotherAwesomeJob.jobInfo)
          retrieved <- jobs.find(retrievedCreate)
        } yield retrieved
        program.asserting(_.map(_.jobInfo) shouldBe Some(AnotherAwesomeJob.jobInfo))
      })
    }
    "should filter remote jobs" in {
      transactor.use(xa => {
        val program = for {
          jobs <- LiveJobs[IO](xa)
          filteredJobs <- jobs.all(JobFilter(remote = true), Pagination.default)
        } yield filteredJobs
        program.asserting(_ shouldBe List())
      })
    }
    "should filter remote by tags" in {
      transactor.use(xa => {
        val program = for {
          jobs <- LiveJobs[IO](xa)
          filteredJobs <- jobs.all(JobFilter(tags = List("scala", "cats", "zio")), Pagination.default)
        } yield filteredJobs
        program.asserting(_ shouldBe List(AwesomeJob))
      })
    }
  }
}
