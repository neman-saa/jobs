package jobsboard.http.validation

import cats.data.{Validated, ValidatedNel}
import jobsboard.domain.job.JobInfo
import jobsboard.domain.auth.{LoginInfo, NewPasswordInfo}
import jobsboard.domain.user.NewUserInfo
import cats.implicits.*
import java.net.URL
import scala.util.{Failure, Success, Try}
import cats.syntax.semigroup.*
object validators {

  trait ValidationFailure {
    def message: String
  }

  private case class EmptyField(fieldName: String) extends ValidationFailure {
    override def message: String = s"field $fieldName is empty"
  }
  private case class InvalidUrl(fieldName: String) extends ValidationFailure {
    override def message: String = s"$fieldName is not a valid URL"
  }
  private case class InvalidEmail(fieldName: String) extends ValidationFailure {
    override def message: String = s"$fieldName is not a valid email"
  }

  case class InvalidPassword(fieldName: String) extends ValidationFailure {
    override def message: String = s"password$fieldName is not a valid password"
  }

  trait Validator[A] {
    def validate(value: A): ValidatedNel[ValidationFailure, A]
  }

  private def validateRequired[A](field: A, fieldName: String)(
      required: A => Boolean
  ): ValidatedNel[ValidationFailure, A] = {
    if (required(field)) field.validNel
    else EmptyField(fieldName).invalidNel
  }

  private def validateUrl(field: String, fieldName: String): ValidatedNel[ValidationFailure, String] =
    Try(URL(field).toURI) match {
      case Success(_) => field.validNel
      case Failure(_) => InvalidUrl(field).invalidNel
    }

  private def validateEmail(field: String, fieldName: String): ValidatedNel[ValidationFailure, String] = {
    val emailRegex =
      """^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r
    if (emailRegex.findFirstMatchIn(field).isDefined) field.validNel
    else InvalidEmail(fieldName).invalidNel
  }

  given jobInfoValidator: Validator[JobInfo] = (jobInfo: JobInfo) => {
    val JobInfo(
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
    ) = jobInfo

    val validCompany     = validateRequired(company, "company")(_.nonEmpty)
    val validTitle       = validateRequired(title, "title")(_.nonEmpty)
    val validDescription = validateRequired(description, "description")(_.nonEmpty)
    val validLocation    = validateRequired(location, "location")(_.nonEmpty)
    val validUrl         = validateUrl(externalUrl, "externalUrl")

    (
      validCompany,
      validTitle,
      validDescription,
      validUrl,
      remote.validNel,
      validLocation,
      salaryLo.validNel,
      salaryHi.validNel,
      currency.validNel,
      country.validNel,
      image.validNel,
      tags.validNel,
      seniority.validNel,
      other.validNel
    ).mapN(JobInfo.apply)
  }

  given loginInfoValidator: Validator[LoginInfo] = (loginInfo: LoginInfo) => {
    val validEmail =
      validateRequired(loginInfo.email, "email")(_.nonEmpty).andThen(e => validateEmail(e, "email"))
    val validPassword = validateRequired(loginInfo.password, "password")(_.nonEmpty)
    (validEmail, validPassword).mapN(LoginInfo.apply)
  }

  given newUserInfoValidator: Validator[NewUserInfo] = (newUserInfo: NewUserInfo) => {
    val validEmail = validateRequired(newUserInfo.email, "email")(_.nonEmpty)
      .andThen(e => validateEmail(e, "email"))
    val validPassword = validateRequired(newUserInfo.password, "password")(_.length > 10)

    (
      validEmail,
      validPassword,
      newUserInfo.firstName.validNel,
      newUserInfo.secondName.validNel,
      newUserInfo.company.validNel
    )
      .mapN(NewUserInfo.apply)
  }

  given newPasswordInfoValidator: Validator[NewPasswordInfo] = (newPasswordInfo: NewPasswordInfo) =>
    {
      val validateNew = validateRequired(newPasswordInfo.newPassword, "new password")(_.length > 10)
      (newPasswordInfo.oldPassword.validNel, validateNew).mapN(NewPasswordInfo.apply)
    }
}
