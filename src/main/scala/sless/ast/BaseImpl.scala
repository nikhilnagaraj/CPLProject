package sless.ast

import sless.dsl._

class BaseImpl extends BaseDSL {
  override type Rule = RuleBase
  override type Css = CssBase
  override type Selector = SelectorBase
  override type Declaration = DeclarationBase
  override type Property = PropertyBase
  override type Value = ValueBase

  override protected def fromRules(rules: Seq[Rule]): Css = CssBase(rules)
}
