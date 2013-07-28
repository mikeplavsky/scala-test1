import org.specs2.mutable._

class LogicSpec extends Specification {

  "matchLiklihood" should {
    "be 100%" in {
      
      val tabby = Kitten("1", List("male", "tabby"))
      val prefs = BuyerPreferences(List("male", "tabby"))
      val result = Logic.matchLiklihood(tabby, prefs)

      result must beGreaterThan(0.999)
    }
    "be 0%" in {
    
      val tabby = Kitten("1", List("male", "tabby"))
      val prefs = BuyerPreferences(List("female", "calico"))
      val result = Logic.matchLiklihood(tabby, prefs)

      result must beLessThan(0.001)

    }

  }

}
