package sless.ast

case class RuleBase(selector: SelectorBase, decls: Seq[DeclarationBase]) {
  var comment: Option[CommentBase] = None
}

object RuleBase {

  def apply(selector: SelectorBase, decls: Seq[DeclarationBase]): RuleBase =
    new RuleBase(selector, decls)

  def addComment(rule:RuleBase, thisComment: CommentBase): RuleBase = {
    rule.comment = Some(thisComment)
    return rule
  }

  def getRuleString(rule: RuleBase) : String = {
    val selectorString: String = SelectorBase.getSelectorString(rule.selector)
    val declStrings: Seq[String] = Utility.map[DeclarationBase,String](DeclarationBase.getDeclarationString,rule.decls)
    if(rule.comment.isDefined){
      val commentString: String = CommentBase.getCommentString(rule.comment.get)
      "/* " + commentString + " */" +  selectorString + "{" + declStrings.mkString("","","") + "}"
    }
    else{
      selectorString + "{" + declStrings.mkString("","","") + "}"
    }
  }

  def getRulePrettyString(rule: RuleBase, spaces: Int) : String = {
    val selectorString: String = SelectorBase.getSelectorPrettyString(rule.selector)
    val declStrings: Seq[String] = Utility.map[DeclarationBase,String](DeclarationBase.getDeclarationPrettyString,rule.decls)
    if(rule.comment.isDefined){
      val commentString: String = CommentBase.getCommentString(rule.comment.get)
      "/* " + commentString + " */" + "\n" + selectorString + " {\n" + declStrings.mkString(" ".repeat(spaces),"\n" + " ".repeat(spaces),"\n") + "}"
    }
    else{
      selectorString + " {\n" + declStrings.mkString(" ".repeat(spaces),"\n" + " ".repeat(spaces),"\n") + "}"
    }


  }

  def isNotEmptyDeclaration(rule: RuleBase): Boolean = rule.decls.nonEmpty

  def marginLintingCheck(rule: RuleBase) : Boolean = {
    var marginTop: Boolean = false
    var marginRight: Boolean = false
    var marginLeft: Boolean = false
    var marginBottom: Boolean = false

    for (decl <- rule.decls) {
      PropertyBase.getPropString(decl.prop) match {
        case "margin-top" => marginTop = true
        case "margin-left" => marginLeft = true
        case "margin-bottom" => marginBottom = true
        case "margin-right" => marginRight = true
        case _ => None
      }
    }

    marginTop && marginBottom && marginLeft && marginRight
  }

  def marginLinting(rule: RuleBase): RuleBase = {
    var declIx: Int = 0
    var marginTop: Option[(Int, String)] = None
    var marginLeft: Option[(Int, String)] = None
    var marginBottom: Option[(Int, String)] = None
    var marginRight: Option[(Int, String)] = None

    for (decl <- rule.decls) {
      PropertyBase.getPropString(decl.prop) match {
        case "margin-top" => marginTop = Some((declIx, ValueBase.getValueString(decl.value)))
        case "margin-left" => marginLeft = Some((declIx, ValueBase.getValueString(decl.value)))
        case "margin-bottom" => marginBottom = Some((declIx, ValueBase.getValueString(decl.value)))
        case "margin-right" => marginRight = Some((declIx, ValueBase.getValueString(decl.value)))
        case _ => None
      }
      declIx += 1
    }
    if (marginTop.nonEmpty && marginLeft.nonEmpty && marginRight.nonEmpty && marginBottom.nonEmpty) {
      val firstMargin = marginTop.get._1 min marginLeft.get._1 min marginRight.get._1 min marginBottom.get._1
      val newValue = marginTop.get._2 + " " + marginRight.get._2 + " " + marginBottom.get._2 + " " + marginLeft.get._2
      val newDecl = DeclarationBase(PropertyBase("margin"),ValueBase(newValue))
      val updatedDecls = rule.decls.updated(firstMargin, newDecl).filter(decl => !PropertyBase.getPropString(decl.prop).startsWith("margin-"))

      RuleBase(rule.selector, updatedDecls)
    }
    else {
      rule
    }
  }
}
