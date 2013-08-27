def fib1(x: Int): Int = {
  if (x == 0 || x == 1) return x
  else fib1(x - 1) + fib1(x - 2)
}
def fib2(num: Int): Int = {
  def loop(num: Int, current: Int, previous: Int): Int = {
    if (num == 0) return previous
    else loop(num - 1, current + previous, current)
  }
  loop(num, 1, 0)
}
val test = fib1(6)
val test2 = fib2(6)
// num: counts from the given number to 0
// current: the current number in the fib sequence
// previous: the previous number in the fib sequence
// loop(6-1, 1+0, 1) = loop(5, 1, 1)
// loop(5-1, 1+1, 1) = loop(4, 2, 1)
// loop(4-1, 2+1, 2) = loop(3, 3, 2)
// loop(3-1, 3+2, 3) = loop(2, 5, 3)
// loop(2-1, 5+3, 5) = loop(1, 8, 5)
// loop(1-1, 8+5, 8) = loop(0, 13, 8)
// loop(0, 13+8, 13) = 13

def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
  b => f(a, b)
}
def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
  a => b => f(a, b)
}
def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
  (a, b) => f(a)(b)
}
def compose[A, B, C](f: B => C, g: A => B): A => C = {
  a => f(g(a))
}