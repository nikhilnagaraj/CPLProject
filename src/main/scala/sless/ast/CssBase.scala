package sless.ast

case class CssBase(rules: Seq[RuleBase])

object CssBase {

  def apply(rules: Seq[RuleBase]): CssBase = new CssBase(rules)
}