package jobsboard.playground

import cats.effect.{IO, IOApp, Resource}
import cats.effect.unsafe.IORuntime
import doobie.*
import doobie.hikari.HikariTransactor
import doobie.implicits.*
import doobie.util.*
import jobsboard.core.LiveJobs
import jobsboard.domain.job.JobInfo
import jobsboard.domain.job.JobInfo
import LiveJobs.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
object JobsPlayground extends IOApp.Simple {

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val postgresResource: Resource[IO, HikariTransactor[IO]] = for {
    ec <- ExecutionContexts.fixedThreadPool(32)
    xa <- HikariTransactor.newHikariTransactor[IO](
      "org.postgresql.Driver",
      "jdbc:postgresql:board",
      "docker",
      "docker",
      ec
    )
  } yield xa

  val jobInfo: JobInfo = JobInfo.minimal(
    company = "Google",
    title = "Software Engineer",
    description = "The best job ever",
    externalUrl = "google.com",
    remote = false,
    location = "NYC"
  )
  override def run = postgresResource.use { xa => for {
    jobs <- LiveJobs[IO](xa)
    _ <- IO.println("Ready. Next...") *> IO.readLine
    id <- jobs.create("google@gmail.com", jobInfo)
    - <- IO.println("Next...") *> IO.readLine
    list <- jobs.all()
    _ <- IO.println(s"All jobs: $list, Next...") *> IO.readLine
    _ <- jobs.update(id, jobInfo.copy(title = "Software rockstar"))
    newJob <- jobs.find(id)
    _ <- IO.println(s"New job: $newJob, Next...") *> IO.readLine
    _ <- jobs.delete(id)
    listAfter <- jobs.all()
    _ <- IO.println(s"Now list should be empty: $listAfter")
  } yield ()
  }
}
