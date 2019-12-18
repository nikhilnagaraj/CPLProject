package sless.ast

case class PropertyBase(thisProp: String)

object PropertyBase {
  def apply(thisProp: String): PropertyBase = new PropertyBase(thisProp)
}
