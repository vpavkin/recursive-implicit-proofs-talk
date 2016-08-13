package reference

case class Container[T](element: T)

object OrderingDemo extends App {

  implicit def containerOrdering[T: Ordering]: Ordering[Container[T]] = Ordering.by(_.element)

  def compare[A](x: A, y: A)(implicit O: Ordering[A]): Int = O.compare(x, y)

  println(compare(
    Container(1),
    Container(2)
  ))

  println(compare(
    Container(Container(1)),
    Container(Container(2))
  ))

  println(compare(
    1 -> Container(Option(Container("a"))),
    1 -> Container(Option(Container("b")))
  ))
}
