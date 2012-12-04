package controllers

import play.api._
import play.api.mvc._

import com.novus.salat._
import com.novus.salat.dao._
import com.mongodb.casbah.Imports._
import se.radley.plugin.salat._
import models.Transaction

import java.util.Date

object Application extends Controller {

  def index = Action {
    Redirect("/transactions")
  }
 
}
