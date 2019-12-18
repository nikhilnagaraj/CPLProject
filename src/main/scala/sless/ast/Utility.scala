package sless.ast

object Utility {

  //Generic map function
  def map[A,B](f: (A) => B, list: Seq[A]): Seq[B] = {
    for {
      x <- list
    } yield f(x)
  }

  //Generic index delete function
  def deleteAtIndex[A](x: Seq[A], ix: Int): Seq[A] = {
    x.take(ix) ++ x.drop(ix + 1)
  }

}
