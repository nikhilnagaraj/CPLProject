package sless.ast

case class DeclarationBase(prop: PropertyBase, value: ValueBase) {
  var comment: Option[CommentBase] = None
}

object DeclarationBase{

  def apply(prop: PropertyBase,value: ValueBase): DeclarationBase = {
    new DeclarationBase(prop,value)
  }

  def addComment(decl: DeclarationBase,thisComment: CommentBase): DeclarationBase = {
    decl.comment = Some(thisComment)
    return decl
  }

  def getDeclarationString(decl: DeclarationBase): String = {
    if(decl.comment.isDefined){
      PropertyBase.getPropString(decl.prop) + ":" + ValueBase.getValueString(decl.value) + ";" +
        "/* " + CommentBase.getCommentString(decl.comment.get) + " */"
    }
    else{
      PropertyBase.getPropString(decl.prop) + ":" + ValueBase.getValueString(decl.value) + ";"
    }

  }

  def getDeclarationPrettyString(decl: DeclarationBase): String = {
    if(decl.comment.isDefined){
      PropertyBase.getPropString(decl.prop) + ": " + ValueBase.getValueString(decl.value) + "; " +
        "/* " + CommentBase.getCommentString(decl.comment.get) + " */"
    }
    else{
      PropertyBase.getPropString(decl.prop) + ": " + ValueBase.getValueString(decl.value) + ";"
    }
  }

}
