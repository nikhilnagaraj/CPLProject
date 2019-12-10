package sless.ast

import sless.dsl.PropertyDSL

trait PropertyBaseTrait extends BaseImpl with PropertyDSL {
  override def prop(string: String): PropertyBase = new PropertyBase(string)
  override protected def assign(p: PropertyBase, value: ValueBase): DeclarationBase = DeclarationBase(p, value)
}
