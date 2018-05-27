package partialsemigroup

import cats.Semigroup
import cats.data.NonEmptyList
import simulacrum.typeclass
import scala.annotation.implicitNotFound

/**
  * A partial semigroup is a semigroup for which `combine` is not required to be defined over all inputs.
  *
  * A PartialSemigroup is like a Semigroup, but with an operator returning [[Option[A]]] rather than [[A]].
  *
  * Comparison:
  *
  *   def combine(x: A, y: A): A
  *   def combineMaybe(x: A, y: A): Option[A]
  *
  * @tparam A
  */
@implicitNotFound("No PartialSemigroup found for type ${A}. Please implement an implicit PartialSemigroup[${A}].")
@typeclass trait PartialSemigroup[A]{

  @simulacrum.op("|+|?", alias = true)
  def combineMaybe(x: A, y: A): Option[A]
}

object PartialSemigroup {

  /**
    * Create a `PartialSemigroup` instance from the given function.
    */
  @inline def instance[A](cmb: (A, A) => Option[A]): PartialSemigroup[A] = new PartialSemigroup[A] {

    override def combineMaybe(x: A, y: A): Option[A] = cmb(x, y)
  }

  /**
    * Create a `PartialSemigroup` instance from the given Semigroup.
    */
  implicit def fromSemigroup[A](implicit ev: Semigroup[A]): PartialSemigroup[A] = new PartialSemigroup[A] {

    override def combineMaybe(x: A, y: A): Option[A] = Some(ev.combine(x, y))
  }


  def groupAndConcat[A](xs: List[A])(implicit ev: PartialSemigroup[A]): List[A] =
    xs match {
      case Nil             => Nil
      case list @ _ :: Nil => list
      case x :: y :: zs    =>
        ev.combineMaybe(x, y) match {
          case Some(a) =>      groupAndConcat(a :: zs)
          case None    => x :: groupAndConcat(y :: zs)
        }
    }


  def partialConcat[A](xs: List[A])(implicit ev: PartialSemigroup[A]): Option[A] =
    NonEmptyList.fromList(xs).flatMap(z => partialConcat1(z))


  def partialConcat1[A](xsNel: NonEmptyList[A])(implicit ev: PartialSemigroup[A]): Option[A] = {
    xsNel match{
      case NonEmptyList(x, Nil)     => Some(x)
      case NonEmptyList(x, y :: zs) => ev.combineMaybe(x, y).flatMap(z => NonEmptyList.fromList(z :: zs)).flatMap(p => partialConcat1(p))
    }
  }


  def partialZip[A](a: List[A], b: List[A])(implicit ev: PartialSemigroup[A]): Option[List[A]] =
    (a, b) match {
      case (Nil, Nil)     => Some(List.empty)
      case (_ :: _, Nil)  => None
      case (Nil, _ :: _)  => None
      case (x::xs, y::ys) =>
        for {
          s1 <- ev.combineMaybe(x, y)
          s2 <- partialZip(xs, ys)
        } yield s1 :: s2
    }


  def partialZip1[A](aNel: NonEmptyList[A], bNel: NonEmptyList[A])(implicit ev: PartialSemigroup[A]): Option[NonEmptyList[A]] =
    (aNel, bNel) match {

      case (NonEmptyList(x, xs), NonEmptyList(y, ys)) =>

        ev.combineMaybe(x, y).flatMap{ z =>

          partialZip(xs, ys).map(zs => z :: zs)

        }.flatMap(l => NonEmptyList.fromList(l))
    }
}

object m {

  import cats.instances.either._


}
