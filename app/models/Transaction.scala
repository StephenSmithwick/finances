package models

import play.api.Play.current
import java.util.{Date}
import com.novus.salat._
import com.novus.salat.dao._
import com.mongodb.casbah.Imports._
import se.radley.plugin.salat._
import salatcontext._

case class Transaction (
  id: ObjectId,
  date: Date,
  memo: String,
  category: String,
  account: String,
  amount: Long 
)

object Transaction extends ModelCompanion[Transaction, ObjectId] {
  val collection = mongoCollection("transactions")
  val dao = new SalatDAO[Transaction, ObjectId](collection = collection) {}
  
  def findTransactionsForAccount(account: String): Iterator[Transaction] = 
    findAll.filter(transaction => transaction.account == account)
    
  override def save(transaction: Transaction) = {
    Account.ensureAccountExists(transaction.account)
    super.save(transaction)
  }
}
