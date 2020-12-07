package functionalprogramming.f_lazy_thunk

sealed trait Stream[+A]

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  /**
   * 构造器接受Thunk参数进行惰性求值,避免初始化时的多次调用
   *
   * @param hd
   * @param tl
   * @tparam A
   * @return
   */
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    List(1, 2, 3).headOption
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  /**
   * as.head, apply(as.tail: _*)都不会求值
   * @param as
   * @tparam A
   * @return
   */
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def headOption[A](stream: Stream[A]): Option[A] = stream match {
    case Empty => None
    case Cons(h, t) => Option(h())
  }

}
