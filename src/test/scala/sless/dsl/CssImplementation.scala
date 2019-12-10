package sless.dsl

import sless.ast.BaseObject

object CssImplementation {
  type DSL = PropertyDSL with SelectorDSL with ValueDSL with Compilable
  val dsl: DSL = BaseObject
}
