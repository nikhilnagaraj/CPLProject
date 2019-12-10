package sless.ast

import sless.dsl.ValueDSL

trait ValueBaseTrait extends BaseImpl with ValueDSL {
  override def value(string: String): ValueBase = {
    new ValueBase(string)
  }
}
