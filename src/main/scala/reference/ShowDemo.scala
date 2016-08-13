package reference

import shapeless.{::, Generic, HList, HNil}

trait Show[A] {
  def show(a: A): String
}

object Show {
  implicit val showString = new Show[String] {
    def show(a: String): String = a
  }

  implicit val showInt = new Show[Int] {
    def show(a: Int): String = a.toString
  }

  implicit val showBoolean = new Show[Boolean] {
    def show(a: Boolean): String = a.toString
  }

  implicit val hnilShow = new Show[HNil] {
    def show(a: HNil): String = ""
  }

  implicit def hconsShow[H, T <: HList](implicit H: Show[H], T: Show[T]) = new Show[H :: T] {
    def show(a: H :: T): String = T.show(a.tail) match {
      case empty if empty.isEmpty => H.show(a.head)
      case nonEmpty => H.show(a.head) + ", " + T.show(a.tail)
    }
  }

  implicit def genericShow[A, Repr](implicit G: Generic.Aux[A, Repr], S: Show[Repr]) = new Show[A] {
    def show(a: A): String = S.show(G.to(a))
  }

  // Aux demo
  /*
  implicit def genericShowAux[A](implicit G: Generic[A], S: Show[G.Repr]): Show[A] = new Show[A] {
    def show(a: A): String = S.show(G.to(a))
  }
  */
}

object ShowDemo extends App {

  def toShowString[A](a: A)(implicit S: Show[A]): String = S.show(a)

  println(toShowString(2 :: "12" :: 3 :: HNil))


  case class Person(name: String, age: Int)
  println(toShowString(Person("Vova", 28)))
}
