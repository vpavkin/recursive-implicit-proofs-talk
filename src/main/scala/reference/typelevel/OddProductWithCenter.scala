package reference.typelevel

import shapeless.ops.hlist.At
import shapeless.test.illTyped
import shapeless.{::, Generic, HList, HNil, Nat, Succ}

trait OddProduct[T] {
  type Index <: Nat
  type Center
  def center(o: T): Center
}

object OddProduct {

  type Aux[T, Index0 <: Nat, Center0] = OddProduct[T] {
    type Index = Index0
    type Center = Center0
  }

  implicit def oneElementHListIsOdd[A]: Aux[A :: HNil, Nat._0, A] = new OddProduct[A :: HNil] {
    type Center = A
    type Index = Nat._0
    def center(o: A :: HNil): A = o.head
  }

  implicit def oddPlus2IsOdd[H1, H2, T <: HList, Ind <: Nat, Cen](
      implicit T: OddProduct.Aux[T, Ind, Cen],
      A: At[H1 :: H2 :: T, Succ[Ind]]
  ): Aux[H1 :: H2 :: T, Succ[Ind], A.Out] = new OddProduct[H1 :: H2 :: T] {
    type Index = Succ[Ind]
    type Center = A.Out
    def center(o: H1 :: H2 :: T): Center = A(o)
  }

  implicit def genericOddProduct[T, Repr](
      implicit G: Generic.Aux[T, Repr],
      O: OddProduct[Repr]
  ): OddProduct[T] = new OddProduct[T] {
    type Index = O.Index
    type Center = O.Center
    def center(o: T): Center = O.center(G.to(o))
  }

}

object OddProductDemo extends App {

  def onlyOdd[T: OddProduct](t: T): Unit = ()

  onlyOdd(1 :: HNil)
  onlyOdd(1 :: "2" :: false :: HNil)
  illTyped("""onlyOdd(1 :: false :: HNil)""")

  case class Odd(a: Int, b: String, c: Boolean)
  case class Even(a: Int, b: String)
  onlyOdd(Odd(1, "2", false))
  illTyped("""onlyOdd(Even(1, "2"))""")


  def center[T](o: T)(implicit O: OddProduct[T]): O.Center =
    O.center(o)

  println(center(Odd(1, "bbbb", false)))
  println(center((1, "3", 2, "3", "5")))
}
