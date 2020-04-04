package Haskell

sealed trait NonEmptyList[+A] {
  val head: A
  val tail: List[A]

  // (x: NonEmptyList[Int], i: Int) => (i <:: x).toList === i :: x
  def <::[B >: A](b: B) = NonEmptyList.nel(b, head :: tail)

  // (x: NonEmptyList[Int], y: List[Int]) => (y <::: x).toList === y ::: x.toList
  def <:::[B >: A](bs: List[B]) = {
    val b = new ListBuffer[B]
    b ++= bs
    b += head
    b ++= tail
    val bb = b.toList
    NonEmptyList.nel(bb.head, bb.tail)
  }

  def :::>[B >: A](bs: List[B]): NonEmptyList[B] = NonEmptyList.nel(head, tail ::: bs)

  def map[B](f: A => B) = NonEmptyList.nel(f(head), tail.map(f))

  def flatMap[B](f: A => NonEmptyList[B])  = {
    val b = new ListBuffer[B]
    val p = f(head)
    b += p.head
    b ++= p.tail
    tail.foreach(a => {
      val p = f(a)
      b += p.head
      b ++= p.tail
    })
    val bb = b.toList
    NonEmptyList.nel(bb.head, bb.tail)
  }

  val toList = head :: tail

  val toStream = Stream.cons(head, tail.projection)

  override def toString = "NonEmpty" + (head :: tail)
}

object NonEmptyList {
  def nel[A](a: A, as: List[A]): NonEmptyList[A] = new NonEmptyList[A] {
    override val head: A = a
    override val tail: List[A] = as
  }

  def nel[A](a: A): NonEmptyList[A] = nel(a, Nil)

  def nels[A](a: A, as: A*): NonEmptyList[A] = nel(a, as.toList)

  def list[A](as: => List[A], e: => NonEmptyList[A]): NonEmptyList[A] = if (as.isEmpty) e else nel(as.head, as.tail)

  def liste[A](as: => List[A], e: => String): NonEmptyList[A] = list(as, error(e))

  def list[A](as: => List[A]): NonEmptyList[A] = list(as, error("list must not be empty"))

  def string(s: => String, e: => NonEmptyList[Char]) = if (s.length == 0) e else {
    val l = s.toList
    nel(l.head, l.tail)
  }

  def stringe(s: => String, e: => String) = string(s, error(e))

  def stringe(s: => String) = string(s, error("list must not be empty"))

  def unapply[A](xs: NonEmptyList[A]): Option[(A, List[A])] = Some(xs.head, xs.tail)

  def apply[A](x: A, xs: A*): NonEmptyList[A] = nel(x, xs.toList)

  def apply[A](x: A, xs: List[A]): NonEmptyList[A] = nel(x, xs)

  implicit def NonEmptyListList[A](xs: NonEmptyList[A]): List[A] = xs.toList

  implicit def NonEmptyListString[A](xs: NonEmptyList[Char]): String = scala.List.toString(xs.toList)

  implicit def NonEmptyListOptionList[A](as: List[A]): Option[NonEmptyList[A]] = as match {
    case Nil => None
    case a :: as => Some(nel(a, as))
  }

  implicit def OptionNonEmptyListList[A](as: Option[NonEmptyList[A]]): List[A] = as match {
    case None => Nil
    case Some(x) => x.head :: x.tail
  }

  def error(m: String) = throw new NoSuchElementException(m)
}
