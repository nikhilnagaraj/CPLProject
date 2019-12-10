package sless.ast

import sless.dsl.Compilable

trait CompilableBaseTrait extends BaseImpl with Compilable {

  override def compile(sheet: CssBase): String = Utility.map[RuleBase, String](RuleBase.getRuleString,sheet.rules).mkString("")

  override def pretty(sheet: CssBase, spaces: Int): String = {
    def facadeRulePrettyString(rule: RuleBase) = RuleBase.getRulePrettyString(rule, spaces)
    Utility.map[RuleBase, String](facadeRulePrettyString,sheet.rules).mkString("\n\n")
  }
}