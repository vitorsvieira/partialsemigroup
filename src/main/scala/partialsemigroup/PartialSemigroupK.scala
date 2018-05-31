package partialsemigroup

import cats.{Semigroup, SemigroupK}
import simulacrum.typeclass
import scala.language.higherKinds
import scala.language.implicitConversions


@typeclass trait PartialSemigroupK[F[_]] { self =>


  @simulacrum.op("<+>?", alias = true)
  def combineMaybeK[A](x: F[A], y: F[A]): Option[F[A]]


  /**
    * Given a type A, create a concrete PartialSemigroup[F[A]].
    *
    * val s: PartialSemigroup[List[Int]] = PartialSemigroupK[List].algebra[Int]
    *
    * @return PartialSemigroup[F[A]]
    */
  def algebra[A]: PartialSemigroup[F[A]] = new PartialSemigroup[F[A]] {

    def combineMaybe(x: F[A], y: F[A]): Option[F[A]] = self.combineMaybeK(x, y)
  }


  /**
   * "Compose" with a `G[_]` type to form a `PartialSemigroupK` for `λ[α => F[G[α]]]`.
   */
  def compose[G[_]]: PartialSemigroupK[λ[α => F[G[α]]]] =
    new ComposedPartialSemigroupK[F, G] {
      val F = self
    }
}

private[partialsemigroup] trait ComposedPartialSemigroupK[F[_], G[_]] extends PartialSemigroupK[λ[α => F[G[α]]]] { outer =>
  def F: PartialSemigroupK[F]

  override def combineMaybeK[A](x: F[G[A]], y: F[G[A]]): Option[F[G[A]]] = F.combineMaybeK(x, y)
}

object PartialSemigroupK{

  /**
    * Create a `PartialSemigroupK` instance from the given SemigroupK.
    */
  implicit def fromSemigroupK[F[_]](implicit ev: SemigroupK[F]): PartialSemigroupK[F] = new PartialSemigroupK[F] {
    def combineMaybeK[A](x: F[A], y: F[A]): Option[F[A]] = Some(ev.combineK(x,y))
  }


  implicit def partialSemigroupKForEither[L](implicit L: PartialSemigroup[L]): PartialSemigroupK[Either[L, ?]] = new PartialSemigroupK[Either[L, ?]] {

    override def combineMaybeK[A](x: Either[L, A], y: Either[L, A]): Option[Either[L, A]] =
      (x,y) match {
        case (Left(xl), Left(yl))   => L.combineMaybe(xl, yl).map(Left.apply)
        case (Right(xr), Right(yr)) => ??? // R.combineMaybe(xr, yr).map(Right.apply)
        case (_, _)                 => None
      }
  }
}
