package sless.dsl

import sless.ast.BaseObject

object ExtendImplementation {
  type DSL = PropertyDSL with SelectorDSL with ValueDSL with ExtendDSL with Compilable
  val dsl: DSL = BaseObject
}
