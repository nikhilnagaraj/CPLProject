package sless.ast

import sless.dsl.LintDSL

trait LintBaseTrait extends BaseImpl with LintDSL {

  /**
    * Check if the given sheet has any style rules without declarations, i.e. of the form "selector {}"
    */
  override def removeEmptyRules(css : Css) : (Boolean, Css) = {
    val filteredRules = css.rules.filter(RuleBase.isNotEmptyDeclaration)
    if(filteredRules.length < css.rules.length) {
      Tuple2[Boolean, Css](true, CssBase(filteredRules))
    } else {
      Tuple2[Boolean, Css](false, css)
    }
  }

  /**
    * Check if the given sheet has any style rules with a  declaration for all four properties from the set
    * margin-left, margin-right, margin-top, and margin-bottom, and if so, replaces each property by
    * the single shorthand property margin. The new margin property takes the place of the first declaration in order of appearance.
    * The values from the individual prorperties are aggregated in the order top-right-bottom-left, with spaces in between.
    */
  override def aggregateMargins(css : Css) : (Boolean, Css) = {
    val ruleChecks = Utility.map[Rule, Boolean](RuleBase.marginLintingCheck, css.rules)
    if(ruleChecks.contains(true)) {
      val newRules = Utility.map[Rule, Rule](RuleBase.marginLinting, css.rules)
      Tuple2[Boolean, Css](true, CssBase(newRules))
    }
    else {
      Tuple2[Boolean, Css](false, css)
    }
  }


  /**
    * Check if the given sheet contains strictly more than n 'float' properties and, if so, returns true, otherwise false.
    */
  def limitFloats(css : Css, n : Integer) : Boolean = {
    var numFloats = 0
    for (rule <- css.rules) {
      for (decl <- rule.decls) {
        if(PropertyBase.getPropString(decl.prop) == "float"){
          numFloats += 1
          if(numFloats > n){
            return true
          }
        }
      }
    }
    false
  }
}
