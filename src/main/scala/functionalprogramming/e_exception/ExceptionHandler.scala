package functionalprogramming.e_exception

object ExceptionHandler {

  /**
   * 非引用透明
   * try代码块中y直接替换为上面定义的异常会被捕获并改变表达式的值
   * 引用透明的表达式并不需要依赖上下文,无论放在哪里都支持替代模型的推导
   * y的定义表达式不是引用透明的同时引入了上下文依赖
   */
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail")
    try {
      val x = 42 + 5
      x + y
    } catch {
      case exception: Exception => 43
    }
  }

  /**
   * 使用Double.NaN或者其他返回值代替异常(不推荐)
   * 1.隐藏了错误的传播,可能导致后续的问题并且很难被发现
   * 2.调用方需要显示的使用if来判断返回值
   * 3.不适用于多态
   */
  def mean(xs: Seq[Double]): Double = {
    //    if (xs.isEmpty) throw new ArithmeticException("mean of empty List")
    if (xs.isEmpty) Double.NaN
    else xs.sum / xs.length
  }

  /**
   * 方法签名添加一个需要由调用者确定的特殊情况返回值
   * 但是调用方必须在调用时就设置好
   */
  def mean(xs: Seq[Double], onEmpty: Double): Double = {
    //    if (xs.isEmpty) throw new ArithmeticException("mean of empty List")
    if (xs.isEmpty) onEmpty
    else xs.sum / xs.length
  }

  /**
   * 参考scala标准库中的Option和Either
   */

  /**
   * Option可选返回
   *
   * @param xs
   * @return
   */
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /**
   * Either互斥并集, 习惯上Right构造器用于表示成功
   *
   * @param xs
   * @return
   */
  def mean(xs: Seq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("xs is empty")
    else
      Right(xs.sum / xs.length)

}
