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
    Ok(views.html.accounts.index(account, accountNames, fetchRunningTotalTransactions(account)))
  }
  
  def index = Action {
    Ok(views.html.accounts.index("All", accountNames, fetchRunningTotalTransactions("")))
  } 
  
  def accountNames = Account.findAll.map(_.name).toList

  def fetchRunningTotalTransactions(account: String) = {
    val transactions =
      if (account.isEmpty()) Transaction.findAll
      else Transaction.findTransactionsForAccount(account)

    (transactions.foldLeft(List[(Transaction, Int)]()) { (running, transaction) =>
      (running match {
        case Nil => (transaction, transaction.amount)
        case (_, total) :: _ => (transaction, total + transaction.amount)
      }) :: running
    }).reverse
  }
  
    
  def postToAccount(account: String) = Action { implicit request =>
    transactionForm.bindFromRequest.fold(
      formWithErrors => BadRequest("Bad Request"),
      transactionForm => {
        Transaction.save(transactionForm.build(new ObjectId, account))
        Redirect(routes.TransactionsController.account(account))
      })
  }

  def post() = Action { implicit request =>
    accountTransactionForm.bindFromRequest.fold(
      formWithErrors => BadRequest("Bad Request"),
      transactionForm => {
        Transaction.save(transactionForm.build(new ObjectId))
        Redirect(routes.TransactionsController.index)
      })
  }

}
