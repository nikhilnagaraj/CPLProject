package sless.ast

import sless.dsl.CommentDSL

trait CommentBaseTrait extends BaseImpl with CommentDSL {
  override protected def commentRule(rule: RuleBase, str: String): RuleBase =
    RuleBase.addComment(rule, Some(CommentBase(str)))

  override protected def commentDeclaration(declaration: DeclarationBase, str: String): DeclarationBase =
    DeclarationBase.addComment(declaration, Some(CommentBase(str)))
}
