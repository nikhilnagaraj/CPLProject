package sless.ast

case class ValueBase(thisValue: String)

object ValueBase {
  def apply(thisValue: String): ValueBase = new ValueBase(thisValue)
}


