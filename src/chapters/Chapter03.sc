sealed trait List[+A]
case object Nil extends List[Nothing] // data constructor for `List`
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match { // Pattern matching example
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  // First try
  def drop[A](list: List[A], n: Int): List[A] = {
    def loop(list: List[A], n: Int): List[A] = {
      if (n < 1) list
      else if (list == Nil) Nil
      else loop(tail(list), n - 1)
    }
    loop(list, n)
  }

  // Take a List and an Int: returns a List
  // inside loop takes same parameters
  // the Int is a counter: if it is less than 1 just return the list
  // else do pattern match
  // if Nil matches, return Nil
  // if a List with a head element and a List tail matches call loop()
  // and pass the tail and the Int - 1 to count down
  def drop2[A](list: List[A], n: Int): List[A] = {
    def loop(list: List[A], n: Int): List[A] = {
      if (n < 1) list
      else list match {
        case Nil => Nil
        case Cons(x, xs) => loop(xs, n - 1)
      }
    }
    loop(list, n)
  }

  // Best implementation
  def drop3[A](list: List[A], n: Int): List[A] = {
    if (n < 1) list
    else list match {
      case Nil => Nil
      case Cons(_, xs) => drop3(xs, n - 1)
    }
  }

  // Removes elements from a List prefix as long as they match a predicate
  def dropWhile[A](list: List[A])(f: A => Boolean): List[A] = list match {
    case Cons(x, xs) if f(x) => dropWhile(xs)(f)
    case _ => list
  }

  // Updates the head
  def setHead[A](list: List[A], h: A): List[A] = list match {
    case Nil => Cons(h, Nil)
    case Cons(_, xs) => Cons(h, xs)
  }
  // ... or ....
  def setHead_1[A](list: List[A])(h: A): List[A] = list match {
    case Nil => Cons(h, Nil)
    case Cons(_, xs) => Cons(h, xs)
  }

  // append a list at the end of another one
  def append[A](list1: List[A], list2: List[A]): List[A] = list1 match {
    case Nil => list2
    case Cons(x, xs) => Cons(x, append(xs, list2))
  }

  def init[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  // foldRight takes type A and B
  // foldRight curries so it:
  //   * takes a list and an initial val
  //   * then takes a function with types A and B and returns B
  //   * finally foldRight returns a B
  def foldRight[A, B](list: List[A], z: B)(f: (A, B) => B): B = list match {
    // base case returns a z: B
    case Nil => z
    // pass the head x: A and foldRight which continues with the tail xs, the
    // value z, and the function to perform f
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  // Fold Left
  def foldLeft[A, B](list: List[A], z: B)(f: (B, A) => B): B = list match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def appendLeft[A](list1: List[A], list2: List[A]): List[A] = list1 match {
    case Nil => list2
    case Cons(x, xs) => foldLeft(list1, list2)((list2, list1) => Cons(x, xs))
  }

  def sum2(list: List[Int]) = foldRight(list, 10)(_ + _)
  def product2(list: List[Int]) = foldRight(list, 1)(_ * _)
  def length[A](list: List[A]): Int = foldRight(list, 0)((_, acc) => acc + 1)
  def sum3(list: List[Int]) = foldLeft(list, 0)(_ + _)
  def product3(list: List[Int]) = foldLeft(list, 1)(_ * _)
  def length3[A](list: List[A]): Int = foldLeft(list, 0)((acc, _) => acc + 1)
  def reverse[A](list: List[A]): List[A] = {
    foldLeft(list, List[A]())((acc, h) => Cons(h, acc))
  }
}



val nums = List(1, 2, 3, 4, 5, 10, 25)
val noms = List(10, 20, 60, 8, 1265)
val nilnums = List()
List.tail(nums)
List.tail(nilnums)

//List.drop3(nums, 3)

List.dropWhile(nums)(_ % 5 != 0)
//
//List.setHead(nums)(100)
//
//List.init(nums)

List.sum2(nums)
//
//List.product2(nums)
//
//List.length(nums)
//
//List.sum3(nums)
//
//List.product3(nums)
//
//List.length(nums)
//
//List.reverse(nums)

List.appendLeft(nums, noms)
