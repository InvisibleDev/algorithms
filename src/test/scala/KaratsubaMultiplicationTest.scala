import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.must.Matchers

class KaratsubaMultiplicationTest extends AnyFlatSpecLike with Matchers{
  it must "multiply numbers correctly as per the lecture" in {
    val (num1, num2) = (1234, 5678)
    KaratsubaMultiplication.multiply(num1, num2) must contain (num1 * num2)
  }

  it must "multiply odd number of digit containing numbers" in {
    val (num1, num2) = (12345, 45678)
    KaratsubaMultiplication.multiply(num1, num2) must contain (num1 * num2)
  }

  it must "not allow to multiply numbers with unequal number of digits" in {
    KaratsubaMultiplication.multiply(123, 2) mustBe empty
  }
}
