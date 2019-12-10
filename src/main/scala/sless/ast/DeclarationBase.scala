package sless.ast

case class DeclarationBase(prop: PropertyBase, value: ValueBase)

object DeclarationBase{

  def apply(prop: PropertyBase,value: ValueBase): DeclarationBase = {
    new DeclarationBase(prop,value)
  }

  def getDeclarationString(decl: DeclarationBase): String = {
    PropertyBase.getPropString(decl.prop) + ":" + ValueBase.getValueString(decl.value)
  }

  def getDeclarationPrettyString(decl: DeclarationBase): String = {
    PropertyBase.getPropString(decl.prop) + ": " + ValueBase.getValueString(decl.value)
  }
}
