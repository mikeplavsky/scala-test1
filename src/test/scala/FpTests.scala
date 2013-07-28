import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class FpTests extends FunSuite with ShouldMatchers {

  import fpinscala.datastructures.{List => L1,Cons,Nil};

  test("drop wile predicat is true") {

    val x = L1(1,2,3,4,5)

    var res = L1.dropWhile(x) {_ < 3 }
    res should equal (L1(3,4,5))

    var res1 = L1.dropWhile(x) {_ > 3 }
    res1 should equal (L1(1,2,3,4,5))

    var res2 = L1.dropWhile(x) {_ > 0 }
    res2 should equal (Nil)
  }

  test("drop n elements") {

    val x = L1(1,2,3,4,5)

    val res = L1.drop(2, x)
    res should equal (L1(3,4,5))

    val res1 = L1.drop(4,x)
    res1 should equal (L1(5))

    val res2 = L1.drop(7,x)
    res2 should equal (Nil)

    val res3 = L1.drop(-7,x)
    res3 should equal (L1(1,2,3,4,5))

  }

  test("tail") {
  
    val x = L1(1,2,3,4)
    val t = L1 tail x

    t should equal (L1(2,3,4))

    val x1 = Nil
    val t1 = L1 tail x1

    t1 should equal (Nil)

  }

  test("matching") {
  
    val x = L1(1,2,3,4,5) match {

      case Cons(x,Cons(2,Cons(4,_))) => x
      case Nil => 42
      case Cons(x,Cons(y,Cons(3,Cons(4,_)))) => x + y
      case Cons(h,t) => h + L1.sum(t)
      case _ => 101

    }

    x should equal (3)
                    
  }

  test("List") {

    val example1 = Cons(1,Cons(2,Cons(3,Nil)))
    val example2 = L1(1,2,3)

    example1 should equal (example2) 

    val total1 = L1.sum(example1)
    total1 should equal (6)

    val total2 = L1.sum(example2)
    total2 should equal (total1)
    
    val example3 = L1(1.0,2.0,3.0)
    val res = L1.product(example3)

    res should equal (6.0)
    example3 should equal (example1)

  }

}
