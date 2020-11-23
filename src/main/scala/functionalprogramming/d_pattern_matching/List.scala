package functionalprogramming.d_pattern_matching

/**
 * 多态List
 *
 * @tparam A 类型参数A前面的加号表示A是发生协变的 => x是y的子类型,那么List[x]是List[y]的子类型
 *
 *           scala> val ex1= Nil
 *           val ex1: functionalprogramming.d_pattern_matching.Nil.type = Nil
 *
 *           scala> val ex2 = Cons(1, Nil)
 *           val ex2 = Cons(1, Nil)val ex2: functionalprogramming.d_pattern_matching.Cons[Int] = Cons(1,Nil)
 *
 *           scala> val ex3 = Cons("a",Cons("b",Nil))
 *           val ex3 = Cons("a",Cons("b",Nil))val ex3: functionalprogramming.d_pattern_matching.Cons[String] = Cons(a,Cons(b,Nil))
 *
 *           val x = List(1, 2, 3, 4, 5) match {
 *           |   case Cons(x, Cons(2, Cons(4, _))) => x
 *           |   case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
 *           |   case Cons(head, tail) => head + sum(tail)
 *           |   case Nil => 0
 *           | }
 *           val x: Int  3
 *
 */
sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

/**
 * List数据类型的伴生对象, object => 单例
 * 通常用来定义创建和处理数据类型的方法
 */
object List {

  def sum(ds: List[Int]): Int = ds match {
    case Nil => 0
    case Cons(head, tail) => head + sum(tail)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(head, tail) => head * product(tail)
  }

  private def foldRight[A, B](ds: List[A], defaultValues: B)(f: (A, B) => B): B = {
    ds match {
      case Nil => defaultValues
      case Cons(head, tail) => f(head, foldRight(tail, defaultValues)(f))
    }
  }

  def sum2(ds: List[Int]) =
    foldRight(ds, 0)((x, y) => x + y)

  def product2(ds: List[Double]) =
    foldRight(ds, 1.0)(_ * _)

  /**
   * 删除头元素
   *
   * @param ds
   * @return
   */
  def deleteHead[A](ds: List[A]): List[A] = ds match {
    case Cons(_, tail) => tail
    case _ => Nil
  }

  /**
   * 合并list
   *
   * @param a1
   * @param a2
   * @tparam A
   * @return
   */
  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(head, tail) => Cons(head, append(tail, a2))
    }

  /**
   * 移除符合f函数的元素(List元素要符合f的顺序)
   *
   * @param as
   * @param f
   * @tparam A
   * @return
   */
  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Cons(head, tail) if f(head) => dropWhile(tail)(f)
      case _ => as
    }
  }

  /**
   * 伴生对象中定义可变参数的apply方法用来构造这个数据类型的实例是一种惯例
   *
   * @param as
   * @tparam A
   * @return
   */
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}
