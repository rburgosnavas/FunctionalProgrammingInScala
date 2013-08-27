package chapters.four

object Option extends App {

  // Option -------------------------------------------------------------------
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      // Will f(a) return null??? Investigate
      case Some(a) => Some(f(a))
    }
    def flatMap[B](f: A => Option[B]): Option[B] = {
      map(f).getOrElse(None)
    }
    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }
    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case None => ob
      case _ => this
    }
    def orElse_2[B >: A](ob: => Option[B]): Option[B] = {

    }
    def orElseWithMap[B >: A](ob: => Option[B]): Option[B] = {
      this map(Some(_)) getOrElse ob
    }
    def filter(f: A => Boolean): Option[A] = this match {
      case _ => None
      case Some(a) if f(a) => this
    }
    def filter_2(f: A => Boolean): Option[A] = ???
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]
  // ---------------------------------------------------------------------- END

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = ???

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(a1 => b.map(b1 => f(a1, b1)))
  }

  import java.util.regex._

  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher_1(pat: String): Option[String => Boolean] =
    for {
      p <- pattern(pat)
    } yield ((s: String) => p.matcher(s).matches)

  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat) map (p => (s: String) => p.matcher(s).matches)

  def doesMatch(pat: String, s: String): Option[Boolean] =
    for {
      p <- mkMatcher_1(pat)
    } yield p(s)

  // EXERCISE 4: Re-implement bothMatch above in terms of this new function, to
  // the extent possible.
  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] = {
    map2(mkMatcher(pat1), mkMatcher(pat2))((f, g) => f(s) && g(s))
  }

  // EXERCISE 5: Write a function sequence, that combines a list of Options
  // into one option containing a list of all the Some values in the original
  // list. If the original list contains None even once, the result of the
  // function should be None, otherwise the result should be Some with a list
  // of all the values.
  //
  // from LU guy
  def sequence_1[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case None :: _ => None
    case Some(h) :: t => sequence_1(t).map(h :: _)
  }

  def parsePatterns(a: List[String]): Option[List[Pattern]] =
    sequence_1(a map pattern)

  // EXERCISE 6: Implement this function. It is straightforward to do using map
  // and sequence, but try for a more efficient implementation that only looks
  // at the list once. In fact, implement sequence in terms of traverse.
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???

  // MAIN ---------------------------------------------------------------------
  override def main(args: Array[String]) {
    val dblSeq = Seq(1.0, 2.0, 5.0, 8.9)
    val result = Option.mean(dblSeq)
    println(result.map(r => r * 100))
  }
}
