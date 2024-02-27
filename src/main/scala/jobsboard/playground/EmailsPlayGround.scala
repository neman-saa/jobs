package jobsboard.playground

import cats.effect.{IO, IOApp}
import jobsboard.configuration.EmailServiceConfig
import jobsboard.core.LiveEmails
import scala.concurrent.duration._
import java.util.Properties
import javax.mail.internet.MimeMessage
import javax.mail.{Authenticator, Message, PasswordAuthentication, Session, Transport}

object EmailsPlayGround extends App {
  /*
  Host	smtp.ethereal.email
  Port	587
  Security	STARTTLS
  Username	aylin.hackett@ethereal.email
  Password	gGvGJYhCnADx1YsJx7
   */

  val host     = "smtp.gmail.com"
  val security = "STARTTLS"
  val username = "anticalka@gmail.com"
  val password = "siga qbxs thrh lmgx"
  val protocol = "smtps"
  /*
  mail.smtp.auth = true
  mail.smtp.starttls.enable = true
  mail.smtp.host = host
  mail.smtp.port = port
  mail.smtp.ssl.trust = host
   */
  private val prop = new Properties

  prop.put("mail.smtp.auth", true)
  prop.put("mail.smtp.starttls.enable", true)
  prop.put("mail.smtp.host", host)
  prop.put("mail.transport.protocol", protocol)

  val auth = new Authenticator {
    override def getPasswordAuthentication: PasswordAuthentication =
      new PasswordAuthentication(username, password)
  }

  private val session = Session.getInstance(prop, auth)

  private val subject = "Илья ебучий хуесос"
  private val content = "<p> лох </p>"

  val message = new MimeMessage(session)
  message.setFrom("neman.saa@gmail.com")
  message.setRecipients(Message.RecipientType.TO, "ilyakhiaruntsaw@gmail.com")
  message.setSubject(subject)
  message.setContent(content, "text/html; charset=utf-8")

  (1 to 1000).foreach(x => {println(x); Thread.sleep(1000); Transport.send(message)})
}

object EmailsEffectPlayground extends IOApp.Simple {
  override def run: IO[Unit] = for {
    emails <- LiveEmails[IO](
      EmailServiceConfig(
        host = "smtp.ethereal.email",
        port = 587,
        user = "aylin.hackett@ethereal.email",
        pass = "gGvGJYhCnADx1YsJx7",
        frontendUrl = "https://google.com"
      )
    )
    _ <- emails.sendPasswordRecoveryEmail("someone@gmail.com", "ABCD!@#$")
  } yield ()
}
