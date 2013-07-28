package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0,_) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = { 
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def dropWhile[A](ls: List[A]) (f: A => Boolean): List[A] = ls match {

    case Nil => Nil
    case Cons(head, tail) if f(head) => dropWhile(tail)(f) 
    case _ => ls

  }

  def drop[A](n: Int, ls: List[A]): List[A] = n match {

    case x if x <= 0 => ls
    case x if x > 0 => drop(x-1, List.tail(ls))

  }

  def tail[A](ls: List[A]): List[A] = ls match {
    case Cons(head, tail) => tail
    case Nil => Nil
  }

}
