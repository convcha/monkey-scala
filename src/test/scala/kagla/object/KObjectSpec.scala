package kagla.`object`

import org.scalatest._

class KObjectSpec extends FlatSpec with Matchers {
  "HashKeys that has same key" should "equal" in {
    val hello1 = KString("Hello World")
    val hello2 = KString("Hello World")
    val diff1 = KString("My name is johnny")
    val diff2 = KString("My name is johnny")

    hello1.hashKey shouldBe hello2.hashKey
    diff1.hashKey shouldBe diff2.hashKey
    hello1.hashKey should not be diff1.hashKey
  }
}
