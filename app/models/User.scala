package models

import play.api.Play.current
import java.util.{Date}
import com.novus.salat._
import com.novus.salat.dao._
import com.mongodb.casbah.Imports._
import se.radley.plugin.salat._
import salatcontext._

case class User(
  id: ObjectId = new ObjectId,
  username: String,
  password: String,
  added: Date = new Date()
)

object User extends ModelCompanion[User, ObjectId] {
  val collection = mongoCollection("users")
  val dao = new SalatDAO[User, ObjectId](collection = collection) {}

  def findOneByUsername(username: String): Option[User] = dao.findOne(MongoDBObject("username" -> username))
}