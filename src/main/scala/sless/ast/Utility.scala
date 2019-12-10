package sless.ast

object Utility {

  def map[A,B](f: (A) => B, list: Seq[A]): Seq[B] = {
    for {
      x <- list
    } yield f(x)
  }

}
