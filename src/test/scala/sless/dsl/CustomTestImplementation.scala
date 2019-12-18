package sless.dsl

import sless.ast.BaseObject

object CustomTestImplementation {
  type DSL = PropertyDSL with SelectorDSL with ValueDSL with ExtendDSL with Compilable
  val dsl: DSL = BaseObject
}
