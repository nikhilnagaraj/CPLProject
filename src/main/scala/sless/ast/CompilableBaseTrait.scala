package sless.ast

import sless.dsl.Compilable

trait CompilableBaseTrait extends BaseImpl with Compilable {

  override def compile(sheet: CssBase): String = {
    val extendedSheet = CssBase.applyExtensions(sheet)
    Utility.map[RuleBase, String](RuleBase.getRuleString, extendedSheet.rules).mkString("")
  }

  override def pretty(sheet: CssBase, spaces: Int): String = {
    val extendedSheet = CssBase.applyExtensions(sheet)
    def facadeRulePrettyString(rule: RuleBase) = RuleBase.getRulePrettyString(rule, spaces)

    Utility.map[RuleBase, String](facadeRulePrettyString, extendedSheet.rules).mkString("\n\n")
  }


}