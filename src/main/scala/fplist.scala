package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def foldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t,f(h,z))(f)
  }

  def foldRight[A,B](l: List[A], z: B)(f: (A,B) => B): B = foldLeft(reverse(l), z)(f)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = foldRight(l, List[B]()) {(x,y) =>
    append(f(x),y)  
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l) {x => 
    f(x) match {
      case false => List[A](x) 
      case true => List[A]()
    }
  }

  def filter1[A](l: List[A])(f: (A) => Boolean): List[A] = foldRight(l, List[A]()) {(x,y) =>
    f(x) match {
      case true => y
      case false => Cons(x,y)
    }
  }

  def add(l: List[Int]) = map(l) {_ + 1}
  def convert(l: List[Double]) = map(l) {_.toString()}

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, List[B]()) {(x,y) => Cons(f(x),y)}

  def foldRight1[A,B](l: List[A], z: B)(f: (A,B) => B): B = l match {
    case Nil => z
    case Cons(h,t) => f(h,foldRight1(t,z)(f))
  }

  def flatten[A](ls: List[List[A]]): List[A] = foldRight(ls, List[A]()) {append(_,_)}

  def reverse[A](ls: List[A]): List[A] = foldLeft(ls, List[A]()) {Cons(_,_)}
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

  def append[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2) { Cons(_,_) }

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
