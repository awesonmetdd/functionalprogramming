package functionalprogramming.c_polymorphism_function

object FindInArray {

  //单态
  def find(ss: Array[String], key: String): Int = {
    def loop(n: Int): Int = {
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)
    }

    loop(0)
  }

  //多态
  def find[A](ss: Array[A], f: A => Boolean): Int = {
    def loop(n: Int): Int = {
      if (n >= ss.length) -1
      else if (f(ss(n))) n
      else loop(n + 1)
    }

    loop(0)
  }


  def andThen[A, B, C](f: A => B, g: B => C): A => C = {
    f andThen g
  }

  def compose[A, B, C](f: A => B, g: B => C): A => C = {
    g compose f
  }


  def main(args: Array[String]): Unit = {
    //高阶函数传入匿名函数
    //函数也是一个对象(x: Int) => x == 3)的类型是Function1
    println(find(Array(1, 2, 3, 4, 5), (x: Int) => x == 3))
  }
}
