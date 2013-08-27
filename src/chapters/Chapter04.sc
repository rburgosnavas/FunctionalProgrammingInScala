sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}


















case class Some[+A](get: A) extends Option[A]


case object None extends Option[Nothing]



object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
}

val dblSeq = Seq(1.0, 2.0, 5.0, 8.9)
val result = Option.mean(dblSeq)
result.map(r => r * 100)