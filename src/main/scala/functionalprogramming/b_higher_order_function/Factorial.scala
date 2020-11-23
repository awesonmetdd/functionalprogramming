package functionalprogramming.b_higher_order_function

object Factorial {

  def factorial(n: Int): Int = {

    //如果递归调用在函数的尾部,scala会自动把递归编译成循环,从而避免每次都要进栈操作
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }

    go(n, 1)
  }


  def formatResult(n: Int, f: Int => Int) = {
    val msg = "the result of %d is %d"
    println(msg.format(n, f(n)))
  }

  def main(args: Array[String]): Unit = {
    formatResult(3, factorial)
  }

}
