package models

import play.api.Play.current
import java.util.{Date}
import com.novus.salat._
import com.novus.salat.dao._
import com.mongodb.casbah.Imports._
import se.radley.plugin.salat._
import salatcontext._

case class Account (
  id: ObjectId,
  name: String
)

object Account extends ModelCompanion[Account, ObjectId] {
  val collection = mongoCollection("accounts")
  val dao = new SalatDAO[Account, ObjectId](collection = collection) {}
  
  def ensureAccountExists(account: String) = {
    val accounts = Account.findAll
    if( ! accounts.exists(_.name == account) ) 
      Account.save(Account(new ObjectId, account))
  }
}
