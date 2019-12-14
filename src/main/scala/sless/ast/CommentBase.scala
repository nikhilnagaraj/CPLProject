package sless.ast

case class CommentBase(thisComment: String)

object CommentBase {

  def apply(thisComment: String): CommentBase = new CommentBase(thisComment)

  def getCommentString(comment: CommentBase): String = comment.thisComment
}
