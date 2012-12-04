package controllers

import play.api._
import play.api.mvc._

import com.novus.salat._
import com.novus.salat.dao._
import com.mongodb.casbah.Imports._
import se.radley.plugin.salat._
import models.{Transaction, Account}

import java.util.Date

import play.api.data._
import play.api.data.Forms._

object TransactionsController extends Controller {

  case class TransactionForm(account: String, date: Date, memo: String, category: String, amount: Int) {
    def build(id: ObjectId) = Transaction(id, date, memo, category, account, amount)
    def build(id: ObjectId, account: String) = Transaction(id, date, memo, category, account, amount)
  }
  def accountTransactionForm = Form(mapping(
    "account" -> text,
    "date" -> date("dd/MM/yyyy"),
    "memo" -> text,
    "category" -> text,
    "amount" -> number)(TransactionForm.apply)(TransactionForm.unapply))

  def transactionForm = Form(mapping(
    "account" -> ignored(""),
    "date" -> date("dd/MM/yyyy"),
    "memo" -> text,
    "category" -> text,
    "amount" -> number)(TransactionForm.apply)(TransactionForm.unapply))

  def account(account: String) = Action {
    Ok(views.html.index(account, accountNames, fetchRunningTotalTransactions(account)))
  }
  
  def index = Action {
    Ok(views.html.index("All", accountNames, fetchRunningTotalTransactions("")))
  }
  
  def accountNames = Account.findAll.map(_.name).toList

  def fetchRunningTotalTransactions(account: String) = {
    val allTransactions = Transaction.findAll.toList.sortWith(_.date before _.date)
    val transactions =
      if (account.isEmpty()) allTransactions
      else allTransactions.filter(transaction => transaction.account == account)

    (transactions.foldLeft(List[(Transaction, Int)]()) { (running, transaction) =>
      (running match {
        case Nil => (transaction, transaction.amount)
        case (_, total) :: _ => (transaction, total + transaction.amount)
      }) :: running
    }).reverse
  }

  def displayTransactions(transactions: List[Transaction]) {
    val transactionWithRunningTotal = transactions.foldLeft(List[(Transaction, Int)]()) { (running, transaction) =>
      (running match {
        case Nil => (transaction, transaction.amount)
        case (_, total) :: _ => (transaction, total + transaction.amount)
      }) :: running
    }
    Ok(views.html.index("All", List("All"), transactionWithRunningTotal.reverse))
  }
  
  def save(transaction: Transaction) = {
    ensureAccountExists(transaction.account)
    Transaction.save(transaction)
  }
  
  def ensureAccountExists(account: String) = {
    val accounts = Account.findAll
    if( ! accounts.exists(_.name == account) ) 
      Account.save(Account(new ObjectId, account))
  }
    
  def postToAccount(account: String) = Action { implicit request =>
    transactionForm.bindFromRequest.fold(
      formWithErrors => BadRequest("Bad Request"),
      transactionForm => {
        save(transactionForm.build(new ObjectId, account))
        Redirect(routes.TransactionsController.account(account))
      })
  }

  def post() = Action { implicit request =>
    accountTransactionForm.bindFromRequest.fold(
      formWithErrors => BadRequest("Bad Request"),
      transactionForm => {
        save(transactionForm.build(new ObjectId))
        Redirect(routes.TransactionsController.index)
      })
  }

}
