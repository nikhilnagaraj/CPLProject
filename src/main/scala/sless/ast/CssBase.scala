package sless.ast

case class CssBase(rules: Seq[RuleBase])

object CssBase {

  def apply(rules: Seq[RuleBase]): CssBase = new CssBase(rules)

  def merge(css1: CssBase, css2: CssBase): CssBase = {

    //Create combined list of selectors
    val selectorList = css1.rules.map(rule => rule.selector) ++ css2.rules.map(rule => rule.selector)
    //Get unique selector list
    val selectorOrderedList = SelectorBase.removeDuplicates(selectorList)
    //Combined rule list
    val rulesList = css1.rules ++ css2.rules

    //Create rules accoring to unique selector list
    var mergedRuleList = Seq[RuleBase]()
    for (selector <- selectorOrderedList) {
      var thisSelectorRule = RuleBase(selector, Seq[DeclarationBase]())
      for (rule <- rulesList) {
        if ((rule.selector == selector || rule.selector.thisSelector.split(",").contains(selector.thisSelector)) && selector != SelectorBase("", "")) {
          for (decl <- rule.decls)
            thisSelectorRule = RuleBase.addDecl(thisSelectorRule, decl, "a")
        }
        thisSelectorRule = RuleBase.addComment(thisSelectorRule, rule.comment)
      }
      mergedRuleList = mergedRuleList :+ thisSelectorRule
    }
    CssBase(mergedRuleList)
  }

  def applyExtensions(css: CssBase): CssBase = {

    //Sequence of all extensions
    val extensions = css.rules.flatMap(rule => rule.selector.extendSelector)

    // Apply extensions to each rule
    var modifiedRules = css.rules
    for (extension <- extensions) {
      var ruleIx = 0
      for (rule <- css.rules) {
        if (rule.selector.thisSelector == extension._2.thisSelector) {
          val extendedSelector = SelectorBase.constructExtension(extension._1, modifiedRules(ruleIx).selector)
          val updatedRule = RuleBase.changeSelector(rule, extendedSelector)
          modifiedRules = modifiedRules.updated(ruleIx, updatedRule)
        }
        ruleIx += 1
      }
    }

    CssBase(modifiedRules)
  }

}