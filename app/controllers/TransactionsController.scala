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
    val transactions = Transaction.findTransactionsForAccount(account)
    Ok(views.html.transactions.show(transactionForm, account, accountNames, addRunningTotal(transactions)))
  }
  
  def postToAccount(account: String) = Action { implicit request =>
    transactionForm.bindFromRequest.fold(
      formWithErrors => BadRequest(accountView(account, formWithErrors)),
      transactionForm => {
        Transaction.save(transactionForm.build(new ObjectId, account))
        Redirect(routes.TransactionsController.account(account))
      })
  }
  
  private def accountView(account: String, form: Form[TransactionForm]) = {
    val transactions = Transaction.findTransactionsForAccount(account)
    views.html.transactions.show(form, account, accountNames, addRunningTotal(transactions))
  }
  
  def index = Action {
    Ok(allTransactionsView(accountTransactionForm))
  }
  
  def post() = Action { implicit request =>
    accountTransactionForm.bindFromRequest.fold(
      formWithErrors => BadRequest(allTransactionsView(formWithErrors)),
      transactionForm => {
        Transaction.save(transactionForm.build(new ObjectId))
        Redirect(routes.TransactionsController.index)
      })
  }
  
  private def allTransactionsView(form: Form[TransactionForm]) = {
    val transactions = Transaction.findAll
    views.html.transactions.index(form, accountNames, addRunningTotal(transactions))
  }
    
  

  
  
  private def accountNames = Account.findAll.map(_.name).toList
  
  private def addRunningTotal(transactions: Iterator[Transaction]) = {
    (transactions.foldLeft(List[(Transaction, Int)]()) { (running, transaction) =>
      (running match {
        case Nil => (transaction, transaction.amount)
        case (_, total) :: _ => (transaction, total + transaction.amount)
      }) :: running
    }).reverse
  }
}
