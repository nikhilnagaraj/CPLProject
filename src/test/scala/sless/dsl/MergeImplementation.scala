package sless.dsl

import sless.ast.{BaseObject, CssBase, RuleBase}


object MergeImplementation {
  type DSL = PropertyDSL with SelectorDSL with ValueDSL with Compilable
  val dsl: DSL = BaseObject

  def mergeSheets(cssSheets: dsl.Css*): dsl.Css = {

    var resultCss = CssBase(Seq[RuleBase]())
    var i = 0
    for (sheet <- cssSheets) {
      resultCss = CssBase.merge(resultCss, sheet.asInstanceOf[CssBase])
      i += 1
    }

    resultCss.asInstanceOf[dsl.Css]
  }
}
