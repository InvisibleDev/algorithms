object KaratsubaMultiplication {

  private def unequalDigitsError(a: Int, b: Int): String = s"Unequal number of digits in a=$a and b=$b"

  def multiply(a: Int, b: Int): Option[Int] = {
    validate(a, b) match {
      case Some(errorMsg) =>
        println(errorMsg)
        None
      case None => Some(mult(a, b).toInt)
    }
  }

  private def mult(num1: Int, num2: Int): Int = {
    if (num1.toString.length == 1 || num2.toString.length == 1)
      num1 * num2
    else {
      val (a, b) = splitNumByHalf(num1.toInt)
      val (c, d) = splitNumByHalf(num2.toInt)
      val n = num1.toString.length

      val ac = mult(a, c)
      val bd = mult(b, d)
      val adPlusBc = mult(a + b, c + d) - ac - bd

      math.pow(10, if (n % 2 == 0) n else n - 1).toInt * ac + math.pow(10, n/2).toInt * adPlusBc + bd
    }
  }

  private def splitNumByHalf(x: Int) = {
    val xStr = x.toString
    val splitAtIndex = if (xStr.length % 2 == 0) xStr.length / 2 else xStr.length / 2 + 1
    val (x1, x2) = xStr.splitAt(splitAtIndex)
    (x1.toInt, x2.toInt)
  }

  private def validate(a: Int, b: Int): Option[String] = {
    if (a.toString.length != b.toString.length)
      Some(unequalDigitsError(a, b))
    else None
  }
}
