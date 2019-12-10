package sless.ast

case class PropertyBase(thisProp: String)

object PropertyBase {
  def getPropString(prop: PropertyBase) : String = {
    prop.thisProp
  }
}
