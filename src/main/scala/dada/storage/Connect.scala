package dada.storage

import dispatch._
import com.ning.http.client.{Cookie, Response}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.JavaConversions._
import scala.annotation.tailrec
import scala.language.postfixOps
import org.json4s.JsonAST.JValue
import org.json4s.DefaultFormats
import dada.Reference
import dada.input.{Input, TcxInput}
import scala.xml.Elem

class Session(c: List[Cookie]) {
  val cookies = c

  @tailrec
  private def addCookie(req: Req, cookies: List[Cookie]): Req =
    cookies match {
      case Nil => req
      case head :: tail => addCookie(req.addCookie(head), tail)
    }

  def <<:(req: Req): Req = addCookie(req, cookies)
}

class Connect(session: Session) {

  def this() = this(null)

  val client = Http

  val urlBase = :/("connect.garmin.com") secure
  val activityServiceBase = urlBase / "proxy" / "activity-service-1.2"
  val searchServiceBase = urlBase / "proxy" / "activity-search-service-1.2"
  val loginUrl = urlBase / "signin"
  val listUrl = searchServiceBase / "json" / "activities"
  val tcxUrl = activityServiceBase / "tcx" / "activity"

  private def asSession(response: Response): Session =
    new Session(response.getCookies().toList)

  // ref. http://www.ciscomonkey.net/gc-to-dm-export/
  def authenticate(username: String, password: String): Option[Connect] = {
    val session = client(loginUrl OK asSession).apply()
    client((loginUrl <<: session).POST <<
      Map("login:loginUsernameField" -> username,
        "login:password" -> password,
        "login" -> "login",
        "login:signInButton" -> "Sign In",
        "javax.faces.ViewState" -> "j_id1") > (resp => asSession(resp))).either() match {
      case Right(_) => Option.apply(new Connect(session))
      case _ => Option.empty
    }
  }

  implicit val formats = DefaultFormats

  case class ConnectActivity(activityId: Number, activityName: String, externalId: String)

  private def toList(json: JValue): List[ConnectActivity] =
    (json \ "results" \ "activities" \ "activity").extract[List[ConnectActivity]]

  def list(limit: Number): List[Reference[dada.Activity]] = {
    require(session != null)
    val listRequest = (listUrl <<: session) <<? Map("start" -> 0.toString, "limit" -> limit.toString)
    val response = client(listRequest OK as.json4s.Json).apply()
    toList(response).map(a => new Reference[dada.Activity](a.activityId))
  }

  def load(activity: Reference[dada.Activity]): Input = {
    require(session != null)
    val loadRequest: Req = (tcxUrl / activity.storageId.toString <<? Map("full" -> "true")) <<: session
    val response: Elem = client(loadRequest OK as.xml.Elem).apply()
    new TcxInput(response)
  }

}
