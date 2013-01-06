package units

import scala.math.max
import scala.math.abs

class Money(val amount: Long, val offset: Long) {
	def *(x: Money) = Money(amount * x.amount, offset * x.offset)
	def +(x: Money) = Money(amount * x.offset + x.amount * offset, x.offset * offset)
	def -(x: Money) = Money(amount * x.offset - x.amount * offset, x.offset * offset)
	def /(x: Money) = Money(amount * x.offset, offset * x.amount)
	 
	def toCents = {
	  val cents = amount * 100 / offset
	  val nextDigit = (amount * 1000 / offset) - cents * 10
	  
	  if(nextDigit > 4) cents + 1 else cents
	}
	  
	override def toString = {
		val dollars = amount/offset
		val cents = toCents - dollars * 100
		val prefix = if (amount < 0 || offset < 0) "-$" else "$"
		prefix + abs(dollars) + "." +  abs(cents)
	}
	
	override def equals(other: Any) = other.isInstanceOf[Money] && 
			other.asInstanceOf[Money].toCents == this.toCents 
}

object Money {
	def apply(amount: Long, offset: Long = 1): Money = {
		val d = gcd(amount, offset)
		new Money(amount/d, offset/d)
	}
	def apply(amount: Double): Money = apply((amount*100).toLong, 100)
	def apply(amount: Float): Money = apply((amount*100).toLong, 100)
	def apply(amount: String): Money = apply(amount.stripPrefix("$").toDouble)
	
	def gcd(x: Long, y: Long): Long = 
		if (y == 0) x else gcd(y, x % y)
}