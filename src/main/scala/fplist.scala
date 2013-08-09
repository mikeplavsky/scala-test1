package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  
  def foldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t,f(h,z))(f)
  }

  def foldRight[A,B](l: List[A], z: B)(f: (A,B) => B): B = l match {
    case Nil => z
    case Cons(h,t) => f(h,foldRight(t,z)(f))
  }

  def length[A](ls: List[A]): Int = foldRight(ls,0) {(x,y) => 1 + y}

  def sum(ints: List[Int]): Int = foldRight(ints, 0){ _ + _ }
  def product(ds: List[Double]): Double = foldRight(ds,1.0) {_ * _}

  def apply[A](as: A*): List[A] = { 
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def init[A](ls: List[A]): List[A] = ls match {
    case Cons(h,Nil) => Nil
    case Cons(h,t) => Cons(h,init(t))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
  
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))

  }

  def setHead[A](ls: List[A], h: A): List[A] = ls match {
    case Nil => Cons(h,Nil)
    case Cons(_,tail) => Cons(h,tail)
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
