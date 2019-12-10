package sless.ast

case class ValueBase(thisValue: String)

object ValueBase {
  def getValueString(valueBase: ValueBase) = valueBase.thisValue
}


