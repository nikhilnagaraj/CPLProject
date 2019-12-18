package sless.ast

case class DeclarationBase(prop: PropertyBase, value: ValueBase) {
  var comment: Option[CommentBase] = None
}

object DeclarationBase{

  def apply(prop: PropertyBase,value: ValueBase): DeclarationBase = {
    new DeclarationBase(prop,value)
  }

  def addComment(decl: DeclarationBase, thisComment: Option[CommentBase]): DeclarationBase = {
    decl.comment = thisComment
    return decl
  }

  def getDeclarationString(decl: DeclarationBase): String = {
    if(decl.comment.isDefined){
      decl.prop.thisProp + ":" + decl.value.thisValue + ";" +
        "/* " + CommentBase.getCommentString(decl.comment.get) + " */"
    }
    else{
      decl.prop.thisProp + ":" + decl.value.thisValue + ";"
    }

  }

  def getDeclarationPrettyString(decl: DeclarationBase): String = {
    if(decl.comment.isDefined){
      decl.prop.thisProp + ": " + decl.value.thisValue + "; " +
        "/* " + CommentBase.getCommentString(decl.comment.get) + " */"
    }
    else{
      decl.prop.thisProp + ": " + decl.value.thisValue + ";"
    }
  }


}
