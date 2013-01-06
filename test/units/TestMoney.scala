package units

import org.specs2.mutable._
import units._

class TestMoney extends Specification { 
  "'Money'" should {
    "be constructable from a String" in {
      Money("12.50") must_== Money(1250,100)
      Money("$12.50") must_== Money(1250,100)
      Money("$12") must_== Money(1200,100)
    }
    "be constructable from a Float" in {
      Money(13.33F) must_== Money(1333,100)
      Money(1.0F) must_== Money(1)
      Money(0.1F) must_== Money(10,100)
    }
    "be constructable from a Double" in {
      Money(13.33D) must_== Money(1333,100)
      Money(1.0D) must_== Money(1)
      Money(0.1D) must_== Money(10,100)
    }
    "determine the nearest cents rounding up" in {
      Money(13.33).toCents must_== 1333
      Money(13335,1000).toCents must_== 1334
      Money(1.001).toCents must_== 100
    }    
    "determine equals by cents" in {
      Money(13.33) must_== Money(1333,100)
      Money(1.001) must_== Money(1)
    }
    "add" in {
      Money(1.10) + Money(0.90) must_== Money(2)
      Money(1.10) + Money(-0.10) must_== Money(1)
      Money(-3.4) + Money(-0.6) must_== Money(-4)
    }
    "subtract" in {
      Money(5) - Money(1) must_== Money(4) 
      Money(3.333) - Money(0.33) must_== Money(3)
      Money(3.4) - Money(-0.6) must_== Money(4)
      Money(-1.10) - Money(-0.10) must_== Money(-1)
    }
    "divide" in {
      Money(10) / Money(5) must_== Money(2)
      Money(10) / Money(3) must_== Money(333,100)
    }
    "multiply" in {
      Money(10) * Money(5) must_== Money(50)
      Money(10) * Money(0.1) must_== Money(1)
    }
    "output with max of 2 decimals points" in {
      Money(10.333).toString must_== "$10.33"
      Money(10.3).toString must_== "$10.30"
    }
    "output negatives correctly" in {
      Money(-10.333).toString must_== "-$10.33"
      val negativeSum = Money(790,100) + Money(-5000,100)
      negativeSum.toString must_== "-$42.10"
    }
  }
}