import org.scalacheck._
import org.scalacheck.Prop._


object StringSpecification extends Properties("String") {

  val startsWith = forAll((a:String, b:String) => (a+b).startsWith(a))
  property("starts with") = startsWith

  val len = forAll((a:String, b:String) =>
    (a+b).length >= a.length && (a+b).length >= b.length)

  property("length") = len
  
  property("length and starts with") = startsWith && len

  property("substring") = forAll((a:String, b:String, c:String) =>
    (a+b+c).substring(a.length,a.length + b.length) == b)

  property("list size") = forAll((l1:List[Int], l2:List[Int]) =>
    l1.size + l2.size == (l1 ::: l2).size )

}

object ExamplesSpecification extends Properties("Examples") {

  property("list size") = forAll((l1:List[Int], l2:List[Int]) =>
    l1.size + l2.size == (l1 ::: l2).size )

  val smallInt = Gen.choose(0,100)
  property("sqrt") = forAll(smallInt)((n:Int) => scala.math.sqrt(n * n) == n)

  property("makeList")  = forAll{n:Int => 
    (n >= 0 && n < 10000) ==> (List.make(n,"").length == n)}

}
