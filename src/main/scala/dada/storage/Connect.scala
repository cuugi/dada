package dada.storage

import dispatch._
import com.ning.http.client.{Cookie, Response}
import scala.concurrent.ExecutionContext.Implicits.global
import org.joda.time.DateTime
import scala.collection.JavaConversions._
import scala.annotation.tailrec
import scala.language.postfixOps

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

class ActivityReference(id: Number, startTime: DateTime) {
}

class Connect(session: Session) {

  def this() = this(null)

  val urlBase = :/("connect.garmin.com") secure
  val searchServiceBase = urlBase / "proxy" / "activity-search-service-1.2"
  val loginUrl = urlBase / "signin"
  val listUrl = searchServiceBase / "json" / "activities"

  def asSession(response: Response): Session =
    new Session(response.getCookies().toList) // .filter(_.getName() == "JSESSIONID"))

  // ref http://www.ciscomonkey.net/gc-to-dm-export/
  def authenticate(username: String, password: String): Connect = {
    val session = Http(loginUrl OK asSession).apply()
    val loginRequest = (loginUrl <<: session) <<
      Map("login:loginUsernameField" -> username,
        "login:password" -> password,
        "login" -> "login",
        "login:signInButton" -> "Sign In",
        "javax.faces.ViewState" -> "j_id1")

    new Connect(Http(loginRequest OK asSession).apply())
  }

  def listActivities(limit: Number): List[ActivityReference] = {
    require(session != null)
    val listRequest = (listUrl <<: session) <<? Map("start" -> 0.toString, "limit" -> limit.toString)
    Http(listRequest OK print).apply()
    Nil
  }
}
