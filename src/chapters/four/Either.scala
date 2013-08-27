package chapters.four

/**
 * Date: 8/25/13
 * Time: 3:31 PM
 */
object Either {

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = ???
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = ???
    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = ???
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = ???
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Double, y: Double): Either[Exception, Double] =
    try {
      Right(x / y)
    } catch {
      case e: Exception => Left(e)
    }
}
