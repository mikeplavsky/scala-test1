case class Player(name:String, score: Int)

object Player {

  def winner(p1: Player, p2: Player) = 
    if (p1.score > p2.score) p1 else p2

  def factorial(n:Int) = {

    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n-1,n*acc)
    }

    go(n,1)
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    s"The ${name} of ${n} is ${f(n)}."
  }

  def partial1[A,B,C](a: A, f: (A,B) => C) : B => C = {
    (b:B) => {f(a,b)}
  }
  
  def curry[A,B,C](f: (A,B) => C): A => B => C = {
    (a:A) => partial1(a,f)
  }

  def uncurry[A,B,C](f: A => B => C): (A,B) => C = {
    (a:A,b:B) => {f(a)(b)}
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a:A) => {f(g(a))}
  }

  def binarySearch[A](as: Array[A], key: A, gt: (A,A) => Boolean): Int = {

    def go(low: Int, mid: Int, high: Int):Int = {

      if (low > high) -mid - 1
      else {
        
        val mid2 = (low + high) / 2

        val a = as(mid2)
        val greater = gt(a,key)

        if(!greater && !gt(key,a)) mid2
        else if (greater) go(low, mid2, mid2 - 1)
        else go(mid2 + 1, mid2, high)

      }
    }

    go(0,0,as.length - 1)

  }

}

object Logic {
  def matchLiklihood(kitten:Kitten, buyer: BuyerPreferences): Double = {

    val matches = buyer.attributes map {
      kitten.attributes contains _
    }

    val nums = matches map {if(_) 1.0 else 0.0}
    nums.sum / nums.length

  }
}
