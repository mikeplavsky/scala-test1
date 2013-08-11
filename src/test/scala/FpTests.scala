import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

class FpTests extends FunSuite with ShouldMatchers {

  import fpinscala.datastructures.{List => L1,Cons,Nil};

  test("hasSubsequence") {

    L1.hasSubsequence(L1(1,2,3,4),L1(2,3)) should equal (true)
    L1.hasSubsequence(L1(1,2,3,4),L1(7,3)) should equal (false)

  } 


  test("zip") {

    L1.zip(L1(1,2,3),L1(2,3,4)) {_ + _} should equal (L1(3,5,7))
    L1.zip(L1(1,2),L1(2,3,4)) {_ + _} should equal (L1(3,5))
    L1.zip(L1(1,2,3),L1(2,5)) {_ + _} should equal (L1(3,7))

    val r = L1.foldRight(L1.zip(L1(1,2,3),L1(2,5)) {_ == _}, true) {_ && _} 
    r should equal (false)
  
    val r1 = L1.foldRight(L1.zip(L1(2,5,3),L1(2,5)) {_ == _}, true) {_ && _} 
    r1 should equal (false)

  }

  test("flatMap") {
  
    L1.flatMap(L1(1,2,3)) {x => L1(x,x)} should equal (L1(1,1,2,2,3,3))

  }

  test("filter") {

    val x = L1(1,2,3,4)
    L1.filter(x) {_>3} should equal (L1(1,2,3))
    L1.filter(x) {_<=2} should equal (L1(3,4))
  
  }

  test("add") {

    val x = L1(1,2,3,4)
    L1.add(x) should equal (L1(2,3,4,5))

    val x1 = L1(0.0,0.1,0.2)
    L1.convert(x1) should equal (L1("0.0","0.1","0.2"))
  
  }

  test("flatten") {

    val x1 = L1(L1(1,2,3),L1(4,5,6),L1(7,8,9))
    L1.flatten(x1) should equal (L1(1,2,3,4,5,6,7,8,9))
  
  }

  test("reverse") {
  
    val x = L1(1,2,3,4)
    L1.reverse(x) should equal (L1(4,3,2,1))

  }

  test("init") {

    val x = L1(1,2,3,4)
    L1.init(x) should equal (L1(1,2,3))

  }


  test("append") {
  
    val x1 = L1(1,2,3)
    val x2 = L1(4,5,6,7)

    L1.append(x1, x2) should equal (L1(1,2,3,4,5,6,7))

  }

  test("setHead") {
  
    val x = L1(1,2,3,4)

    val res = L1.setHead(x,5)
    res should equal (L1(5,2,3,4))

    L1.setHead(Nil,1) should equal (L1(1))

  }


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

  test("Length") {
  
    val x = L1(1,2,3,4,5)
    L1.length(x) should equal (5)

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
