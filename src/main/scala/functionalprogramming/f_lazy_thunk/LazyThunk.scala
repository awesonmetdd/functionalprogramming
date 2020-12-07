package functionalprogramming.f_lazy_thunk

object LazyThunk {

  /**
   * i参数是一个惰性求值表达式,只有使用到时才会进行计算
   * 下面示例会计算两次
   *
   * @param b
   * @param i
   * @return
   */
  def calTwice(b: Boolean, i: => Int) = if (b) i + i else 0

  /**
   * lazy关键字显示缓存i的值后只进行计算一次
   * 对一个val声明的变量添加lazy修饰符将导致scala延迟对这个变量求值,知道它第一次被引用的地方,同时会缓存结果
   *
   * @param b
   * @param i
   * @return
   */
  def lazyCalOnce(b: Boolean, i: => Int) = {
    lazy val j = i
    if (b) j + j else 0
  }

  def main(args: Array[String]): Unit = {
    calTwice(true, {
      println("计算求值calTwice")
      22 + 1
    })

    lazyCalOnce(true, {
      println("计算求值calOnce")
      22 + 2
    })
  }

}
