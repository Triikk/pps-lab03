package it.unibo.pps.u03.part3

import u03.Sequences.Sequence
import u03.Sequences.Sequence.*
import u03.Streams.Stream
import u03.Streams.Stream.cons
import u03.Streams.Stream.empty

object ex5 {

  def cycle[A](lst : Sequence[A]) : Stream[A] =
    def cycleRec(lst: Sequence[A], curr : Sequence[A]) : Stream[A] = (lst,curr) match {
      case (Nil(),_) => empty()
      case (Cons(h,t),Nil()) => cons(h,cycleRec(lst,t))
      case (lst,Cons(h,t)) => cons(h,cycleRec(lst,t))
    }
    cycleRec(lst,lst)

  @main def ex5p4 = {
    val innovativeStream = cycle(Cons('a', Cons('b', Cons('c', Nil()))))
    println(Stream.toList(Stream.take(innovativeStream)(10)))
  }
}
