package reference

import shapeless.test.illTyped
import shapeless.{::, Generic, HList, HNil}

trait OddProduct[T]

object OddProduct {

  implicit def oneElementHListIsOdd[A]: OddProduct[A :: HNil] = new OddProduct[A :: HNil] {}

  implicit def oddPlus2IsOdd[H1, H2, T <: HList](implicit T: OddProduct[T]): OddProduct[H1 :: H2 :: T] =
    new OddProduct[H1 :: H2 :: T] {}

  implicit def genericOddProduct[T, Repr](
      implicit G: Generic.Aux[T, Repr],
      O: OddProduct[Repr]
  ): OddProduct[T] = new OddProduct[T] {}

}

object OddProductDemo extends App {

  def onlyOdd[T: OddProduct](t: T): Unit = ()

  onlyOdd(1 :: HNil)
  onlyOdd(1 :: "a" :: false :: HNil)
  onlyOdd(1 :: "a" :: true :: 2 :: 3 :: HNil)
  illTyped("""onlyOdd(1 :: false :: HNil)""")

  case class Odd(a: Int, b: String, c: Boolean)
  case class Even(a: Int, b: String)
  onlyOdd(Odd(1, "2", false))
  illTyped("""onlyOdd(Even(1, "2"))""")

}
