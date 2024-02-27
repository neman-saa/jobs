package jobsboard.core

import jobsboard.domain.job.{Job, JobFilter, JobInfo}
import cats.effect.Concurrent
import cats.implicits.*
import jobsboard.domain.pagination.*
import jobsboard.logging.syntax.*
import java.util.UUID
import doobie.*
import doobie.implicits.*
import doobie.postgres.implicits.*
import doobie.util.*
import jobsboard.domain.job.{Job, JobInfo}
import org.typelevel.log4cats.Logger

trait Jobs[F[_]] {
  def create(ownerEmail: String, jobsInfo: JobInfo): F[UUID]
  def all(): F[List[Job]]
  def all(filter: JobFilter, pagination: Pagination): F[List[Job]]
  def find(id: UUID): F[Option[Job]]
  def update(id: UUID, jobInfo: JobInfo): F[Option[Job]]
  def delete(id: UUID): F[Int]
}
/*
id: UUID,
date: Long,
ownerEmail: String,
jobInfo: JobInfo,
active: Boolean = false
company: String,
title: String,
description: String,
externalUrl: String,
remote: Boolean,
location: String,
salaryLo: Option[Int],
salaryHi: Option[Int],
currency: Option[String],
country: Option[String],
image: Option[String],
tags: Option[List[String]],
seniority: Option[String],
other: Option[String]
 */
private class LiveJobs[F[_]: Concurrent: Logger](xa: Transactor[F]) extends Jobs[F] {
  override def create(ownerEmail: String, jobsInfo: JobInfo): F[UUID] =
    sql"""
    INSERT INTO jobs(
    date,
    ownerEmail,
    active,
    company,
    title,
    description,
    externalUrl,
    remote,
    location,
    salaryLo,
    salaryHi,
    currency,
    country,
    image,
    tags,
    seniority,
    other
       ) VALUES (
       ${System.currentTimeMillis()},
       $ownerEmail,
       false,
       ${jobsInfo.company},
       ${jobsInfo.title},
       ${jobsInfo.description},
       ${jobsInfo.externalUrl},
       ${jobsInfo.remote},
       ${jobsInfo.location},
       ${jobsInfo.salaryLo},
       ${jobsInfo.salaryHi},
       ${jobsInfo.currency},
       ${jobsInfo.country},
       ${jobsInfo.image},
       ${jobsInfo.tags},
       ${jobsInfo.seniority},
       ${jobsInfo.other}
       )""".update
      .withUniqueGeneratedKeys[UUID]("id")
      .transact(xa)
  override def all(): F[List[Job]] =
    sql"SELECT * FROM jobs"
      .query[Job]
      .to[List]
      .transact(xa)

  override def all(filter: JobFilter, pagination: Pagination): F[List[Job]] = {

    val selectFragment = fr"SELECT *"
    val fromFragment   = fr"FROM jobs"
    val whereFragment = Fragments.whereAndOpt(
      filter.companies.toNel.map(companies => Fragments.in(fr"company", companies)),
      filter.locations.toNel.map(locations => Fragments.in(fr"location", locations)),
      filter.countries.toNel.map(countries => Fragments.in(fr"country", countries)),
      filter.seniorities.toNel.map(seniorities => Fragments.in(fr"seniority", seniorities)),
      filter.tags.toNel.map(tags => Fragments.or(tags.toList.map(tag => fr"$tag=any(tags)"): _*)),
      filter.maxSalary.map(salary => fr"salaryHi > $salary"),
      filter.remote.some.map(remote => fr"remote = $remote")
    )
    val paginationFragment =
      fr"ORDER BY id LIMIT ${pagination.limit} OFFSET ${pagination.offset}"

    val statement = selectFragment |+| fromFragment |+| whereFragment |+| paginationFragment

    Logger[F].info(statement.toString) *>
      statement
        .query[Job]
        .to[List]
        .transact(xa)
        .logError(e => s"failed query: $e")
  }

  override def find(id: UUID): F[Option[Job]] =
    sql"SELECT * FROM jobs WHERE jobs.id = $id"
      .query[Job]
      .option
      .transact(xa)
  override def update(id: UUID, jobInfo: JobInfo): F[Option[Job]] =
    sql"""
    UPDATE jobs
    SET
    company = ${jobInfo.company},
    title = ${jobInfo.title},
    description = ${jobInfo.description},
    externalUrl = ${jobInfo.externalUrl},
    remote = ${jobInfo.remote},
    location = ${jobInfo.location},
    salaryLo = ${jobInfo.salaryLo},
    salaryHi = ${jobInfo.salaryHi},
    currency = ${jobInfo.currency},
    country = ${jobInfo.country},
    image = ${jobInfo.image},
    tags = ${jobInfo.tags},
    seniority = ${jobInfo.seniority},
    other = ${jobInfo.other}
    WHERE id = $id
         """.update.run.transact(xa).flatMap(_ => find(id))
  override def delete(id: UUID): F[Int] =
    sql"""
         DELETE FROM jobs
         WHERE id = $id
         """.update.run.transact(xa)

}

object LiveJobs {
  given jobRead: Read[Job] = Read[
    (
        UUID,
        Long,
        String,
        Boolean,
        String,
        String,
        String,
        String,
        Boolean,
        String,
        Option[Int],
        Option[Int],
        Option[String],
        Option[String],
        Option[String],
        Option[List[String]],
        Option[String],
        Option[String]
    )
  ].map {
    case (
          id: UUID,
          date: Long,
          ownerEmail: String,
          active: Boolean,
          company: String,
          title: String,
          description: String,
          externalUrl: String,
          remote: Boolean,
          location: String,
          salaryLo: Option[Int],
          salaryHi: Option[Int],
          currency: Option[String],
          country: Option[String],
          image: Option[String],
          tags: Option[List[String]],
          seniority: Option[String],
          other: Option[String]
        ) =>
      Job(
        id = id,
        date = date,
        ownerEmail = ownerEmail,
        active = active,
        JobInfo(
          company = company,
          title = title,
          description = description,
          externalUrl = externalUrl,
          remote = remote,
          location = location,
          salaryLo = salaryLo,
          salaryHi = salaryHi,
          currency = currency,
          country = country,
          image = image,
          tags = tags,
          seniority = seniority,
          other = other
        )
      )
  }

  def apply[F[_]: Concurrent: Logger](xa: Transactor[F]): F[LiveJobs[F]] =
    new LiveJobs[F](xa).pure[F]
}
