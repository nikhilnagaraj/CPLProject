package sless.ast

import sless.dsl.ExtendDSL

trait ExtendBaseTrait extends BaseImpl with ExtendDSL {
  override protected def extendI(s: SelectorBase, selector: SelectorBase): SelectorBase = {
    SelectorBase(s.thisSelector, s.thisPrettySelector, s.extendSelector :+ (s, selector))
  }
}
