import cats.data.EitherT
import cats.instances.future._

import scala.concurrent.{ExecutionContext, Future}

object Async {

  case class ErrorMessage(value: String) extends AnyVal

  case class Thing(value: String) extends AnyVal

  def findUpdateAudit(implicit ec: ExecutionContext): Future[Either[ErrorMessage, Thing]] = {
    findAThing.flatMap {
      case Some(thing) => upsertAThing(thing).collect {
        case Right(updatedThing) => Right(auditAThing(updatedThing))
      }
      case None => Future.successful(Left(ErrorMessage("Thing not found")))
    }
  }

  def findUpdateAuditWithCats(implicit ec: ExecutionContext): Future[Either[ErrorMessage, Thing]] = {
    val f = for {
      thing <- EitherT.fromOptionF[Future,ErrorMessage,Thing](findAThing, ErrorMessage("Thing not found"))
      updatedThing <- EitherT[Future,ErrorMessage,Thing](upsertAThing(thing))
      auditedThing <- EitherT.pure[Future,ErrorMessage](auditAThing(updatedThing))
    } yield auditedThing

    f.value
  }

  def findAThing: Future[Option[Thing]] = {
    //find a thing and if found...
    Future.successful(Some(Thing("ABC123")))
  }

  def upsertAThing(thing: Thing): Future[Either[ErrorMessage, Thing]] = {
    //store the thing and then if successful..
    Future.successful(Right(thing))
  }

  def auditAThing(thing: Thing): Thing = {
    //do some auditing
    thing
  }
}
