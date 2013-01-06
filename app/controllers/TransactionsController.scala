package controllers

import play.api._
import play.api.mvc._

import com.novus.salat._
import com.novus.salat.dao._
import com.mongodb.casbah.Imports._
import se.radley.plugin.salat._
import models.{Transaction, Account}
import units._
import views.TransactionView

import java.util.Date

import play.api.data._
import play.api.data.Forms._

object TransactionsController extends Controller {

  case class TransactionForm(account: String, date: Date, memo: String, category: String, amount: String) {
    def build(id: ObjectId) = Transaction(id, date, memo, category, account, Money(amount).toCents)
    def build(id: ObjectId, account: String) = Transaction(id, date, memo, category, account, Money(amount).toCents)
  }
  
  def accountTransactionForm = Form(mapping(
    "account" -> text,
    "date" -> date("dd/MM/yyyy"),
    "memo" -> text,
    "category" -> text,
    "amount" -> text)(TransactionForm.apply)(TransactionForm.unapply))

  def transactionForm = Form(mapping(
    "account" -> ignored(""),
    "date" -> date("dd/MM/yyyy"),
    "memo" -> text,
    "category" -> text,
    "amount" -> text)(TransactionForm.apply)(TransactionForm.unapply))

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
  
  private def addRunningTotal(transactions: Iterator[Transaction]): List[(TransactionView, Money)] = {
    (transactions.foldLeft(List[(TransactionView, Money)]()) { (running, transaction) =>
      (running match {
        case Nil => (TransactionView(transaction), Money(transaction.amount, 100))
        case (_, total) :: _ => (TransactionView(transaction), total + Money(transaction.amount, 100))
      }) :: running
    }).reverse
  }
}
