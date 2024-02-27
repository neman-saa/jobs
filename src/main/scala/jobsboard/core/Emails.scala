package jobsboard.core

import cats.implicits.*
import cats.effect.*
import jobsboard.configuration.EmailServiceConfig

import java.util.Properties
import javax.mail.internet.MimeMessage
import javax.mail.{Authenticator, Message, PasswordAuthentication, Session, Transport}
trait Emails[F[_]] {
  def sendEmail(to: String, subject: String, content: String): F[Unit]
  def sendPasswordRecoveryEmail(to: String, token: String): F[Unit]
}

class LiveEmails[F[_]: MonadCancelThrow](emailServiceConfig: EmailServiceConfig) extends Emails[F] {

  val host: String        = emailServiceConfig.host
  val port: Int           = emailServiceConfig.port
  val user: String        = emailServiceConfig.user
  val pass: String        = emailServiceConfig.pass
  val frontendUrl: String = emailServiceConfig.frontendUrl
  override def sendPasswordRecoveryEmail(to: String, token: String): F[Unit] = {
    val subject = "Jobs: password recovery"
    val content =
      s"""
         |<div style="
         |boarder: 1px solid black;
         |padding: 20px;
         |font-family: sans-serif;
         |line-height: 2;
         |font-size: 20px;
         |">
         |<h1> Jobs: Password recovery </h1>
         |<p> Your password recovery token is $token </p>
         |<p> Click <a href="$frontendUrl/login"> here </a> to get back to the application </p>
         |</div>
         |""".stripMargin
    sendEmail(to, subject, content)
  }
  override def sendEmail(to: String, subject: String, content: String): F[Unit] = {
    val messageResource = for {
      prop    <- propResource
      auth    <- authenticationResource
      session <- createSession(prop, auth)
      message <- createMessage(session)("al@gmail.com", to, subject, content)
    } yield message

    messageResource.use(msg => Transport.send(msg).pure[F])
  }

  private val propResource: Resource[F, Properties] = {
    val prop = new Properties
    prop.put("mail.smtp.auth", true)
    prop.put("mail.smtp.starttls.enable", true)
    prop.put("mail.smtp.host", host)
    prop.put("mail.smtp.port", port)
    prop.put("mail.smtp.ssl.trust", host)
    Resource.pure(prop)
  }

  private val authenticationResource: Resource[F, Authenticator] = Resource.pure(new Authenticator {
    override def getPasswordAuthentication: PasswordAuthentication =
      new PasswordAuthentication(user, pass)
  })

  private def createSession(prop: Properties, auth: Authenticator): Resource[F, Session] =
    Resource.pure(Session.getInstance(prop, auth))

  private def createMessage(
      session: Session
  )(from: String, to: String, subject: String, content: String): Resource[F, MimeMessage] = {
    val message = new MimeMessage(session)
    message.setFrom(from)
    message.setRecipients(Message.RecipientType.TO, "the.user@gmail.com")
    message.setSubject(subject)
    message.setContent(content, "text/html; charset=utf-8")

    Resource.pure(message)
  }

}

object LiveEmails {
  def apply[F[_]: MonadCancelThrow](emailServiceConfig: EmailServiceConfig): F[LiveEmails[F]] =
    new LiveEmails[F](emailServiceConfig).pure[F]
}
