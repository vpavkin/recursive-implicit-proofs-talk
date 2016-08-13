package reference

import cats.Eq
import shapeless.ops.hlist.ToTraversable
import shapeless.{::, Generic, HList, HNil}

sealed trait Difference[+A]
case object Same extends Difference[Nothing]
case class Different[A](x: A, y: A) extends Difference[A]

trait Diffable[A] {
  type Diff
  def diff(x: A, y: A): Diff
}

object Diffable {

  type Aux[A, Diff0] = Diffable[A] {type Diff = Diff0}

  implicit val hNilDiffable: Diffable.Aux[HNil, HNil] = new Diffable[HNil] {
    type Diff = HNil
    def diff(x: HNil, y: HNil): HNil = HNil
  }

  implicit def hConsDiffable[H, T <: HList, HDiff, TDiff <: HList](
      implicit
      H: Diffable.Aux[H, HDiff],
      T: Diffable.Aux[T, TDiff]): Diffable.Aux[H :: T, HDiff :: TDiff] = new Diffable[H :: T] {

    type Diff = HDiff :: TDiff

    def diff(x: H :: T, y: H :: T): HDiff :: TDiff = H.diff(x.head, y.head) :: T.diff(x.tail, y.tail)
  }

  implicit def genDiffable[T, Repr](
      implicit
      G: Generic.Aux[T, Repr],
      D: Diffable[Repr]): Diffable.Aux[T, D.Diff] = new Diffable[T] {
    type Diff = D.Diff
    def diff(x: T, y: T): D.Diff = D.diff(G.to(x), G.to(y))
  }

  implicit def eqBasedDiffable[T](implicit EQ: Eq[T]): Diffable.Aux[T, Difference[T]] = new Diffable[T] {
    type Diff = Difference[T]

    def diff(x: T, y: T): Difference[T] =
      if (EQ.eqv(x, y)) Same
      else Different(x, y)

  }
}

object DiffDemo extends App {
  case class Person(name: String, age: Int, married: Boolean)

  import cats.std.all._

  def diff[T](x: T, y: T)(implicit D: Diffable[T]): D.Diff = D.diff(x, y)

  def prettyDiff[Diff <: HList](
      d: Diff)(
      implicit toTraversableAux: ToTraversable.Aux[Diff, List, Difference[_]]) =
    d.toList[Difference[_]].map {
      case Same => "*"
      case Different(x, y) => s"$x vs. $y"
    }.mkString(",")

  val a = diff(Person("Leo", 12, false), Person("Vova", 12, true))
  val b = diff("Vova" :: 12 :: HNil, "Vova" :: 13 :: HNil)

  println(a)
  println(b)

  println(prettyDiff(a))
  println(prettyDiff(b))
}
