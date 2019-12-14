package sless.dsl

import sless.ast.BaseObject

object CommentImplementation {
  type DSL = PropertyDSL with SelectorDSL with ValueDSL with CommentDSL with Compilable
  val dsl: DSL = BaseObject
}
