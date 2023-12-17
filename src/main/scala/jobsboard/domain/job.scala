package jobsboard.domain

import java.util.UUID

object job {

  case class Job(
      id: UUID,
      date: Long,
      ownerEmail: String,
      active: Boolean = false,
      jobInfo: JobInfo
  )

  case class JobInfo(
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
  )

  object JobInfo {
    val empty: JobInfo =
      JobInfo("", "", "", "", false, "", None, None, None, None, None, None, None, None)
    def minimal(
        company: String,
        title: String,
        description: String,
        externalUrl: String,
        remote: Boolean,
        location: String
    ): JobInfo =
      JobInfo(
        company = company,
        title = title,
        description = description,
        externalUrl = externalUrl,
        remote = remote,
        location = location,
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        None
      )
  }

  final case class JobFilter(
      companies: List[String] = Nil,
      locations: List[String] = Nil,
      countries: List[String] = Nil,
      seniorities: List[String] = Nil,
      tags: List[String] = Nil,
      maxSalary: Option[Int] = None,
      remote: Boolean = false
  )
}
