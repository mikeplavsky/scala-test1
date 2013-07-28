import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class PlayerTests extends FunSuite with ShouldMatchers {

  test("winner function") {
    
    val p1 = Player("Mary", 7)
    val p2 = Player("Bob", 10)

    assert(Player.winner(p1,p2) === p2)

    val p3 = Player("Nick", 1)
    assert(Player.winner(p3,p1) === p1)

  }

  test("curry") {
  
    def inc(a:Int,b:Int) = a + b
    val z = Player.curry(inc)
    
    val z5 = z(5)
    z5(2) should equal (7)

    val inc1 = Player.uncurry(z)
    inc1(3,7) should equal (10)

  }

  test("compose") {

    val inc3 = (a:Int) => a + 3
    val diff4 = (a:Int) => a - 4

    val z = Player.compose(inc3, diff4)
    z(12) should equal (11)

    val z1 = inc3 compose diff4 andThen {_ / 3}
    z1(12) should equal (3)
  
  }

  test("partial") {
  
    def inc(a:Int,b:Int) =  a + b
    inc(1,2) should equal (3)

    val z = Player.partial1(1,inc)
    z(4) should equal (5)

    def log( m:String, a: Int) = m.format(a)
    val r = Player.partial1("result = %d",log)

    r(12) should include ("12")

  }

  test("binary search") {
    
    Player.binarySearch(Array("a","b","c"), "c", (x:String,y:String) => x > y) should equal (2)
    Player.binarySearch(Array(3,4,5), 3, (x:Int,y:Int) => x > y) should equal (0)
    Player.binarySearch(Array(3), 7, (x:Int,y:Int) => x > y) should equal (-1)
    Player.binarySearch(Array(3,4,5), 7, (x:Int,y:Int) => x > y) should equal (-3)
    Player.binarySearch(Array(3.0,4.0,5.0), 1.0, (x:Double,y:Double) => x > y) should equal (-1)
  
  }

  test("formatResult") {
  
    val res = Player.formatResult("factorial",12, Player.factorial)
    assert(res === "The factorial of 12 is 479001600.")

    Player.formatResult("inc",5,_ + 1) should include ("is 6")
    Player.formatResult("inc",7,x => {val r = x + 1;r}) should include ("is 8")

    object T extends Function1[Int,Int]{
      def apply(x:Int):Int = {x + 2}
    }
    
    Player.formatResult("inc", 9, T) should include ("11")

  }

  test("several players") {

    val players = List(
        Player("Sue", 7),
        Player("Bob", 8),
        Player("Joe",4)
      )

    val p = players.reduceLeft(Player.winner)
    assert(p === Player("Bob", 8))
  
  }

  test("factorial") {

    assert(Player.factorial(5) === 120)
    assert(Player.factorial(12) === 479001600)
  
  }

}

class LogicTests extends FunSuite {

  val tabby = Kitten("1", List("male", "tabby"))

  test("100% when all attributes match"){

    val prefs = BuyerPreferences(List("male", "tabby"))
    val result = Logic.matchLiklihood(tabby, prefs)

    assert(result === 1.0)

  }

  test("0% when all attributes do not match"){

    val prefs = BuyerPreferences(List("female", "soft"))
    val result = Logic.matchLiklihood(tabby, prefs)

    assert(result === 0.0)

  }

  test("0.5% when attributes half match"){

    val prefs = BuyerPreferences(List("male", "soft"))
    val result = Logic.matchLiklihood(tabby, prefs)

    assert(result === 0.5)

  }
}
