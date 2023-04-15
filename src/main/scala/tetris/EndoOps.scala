package tetris

import cats._

object EndoOps {

  def repeat[A](ea: Endo[A], n: Int): Endo[A] = Foldable[List].foldK[Endo, A](List.fill(n)(ea))

  implicit class EndoOps[A](val ea: Endo[A]) {
    def ^(n: Int): Endo[A] = repeat(ea, n)
  }

}
