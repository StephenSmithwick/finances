package views

import units._
import java.util.{Date}
import models.Transaction

class TransactionView (
  val date: Date,
  val memo: String,
  val category: String,
  val account: String,
  val amount: Money 
)

object TransactionView {
  def apply(t: Transaction) = new TransactionView(t.date, t.memo, t.category, t.account, Money(t.amount, 100))
}