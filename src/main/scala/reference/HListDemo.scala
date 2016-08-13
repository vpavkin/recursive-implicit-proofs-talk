package reference

import shapeless.test.illTyped
import shapeless.{Generic, HNil}

object HListDemo extends App {

  val h = 1 :: "a" :: true :: HNil
  val l = List(1, "a", true)

  h.head
  l.head
  h.tail
  h.tail.tail.tail
  h.tail.tail.head
  illTyped("""a.tail.tail.tail.tail""")
  illTyped("""a.tail.tail.tail.head""")



  h(0)
  l(0)
  h(1)
  l(1)
  h(2)
  illTyped("""a(3)""")

  case class Person(name: String, age: Int)

  val gen = Generic[Person]
  val p = Person("Brad", 30)

  val repr = gen.to(p)
  gen.from("Pitt" :: 32 :: HNil)
  gen.from(repr)
}
