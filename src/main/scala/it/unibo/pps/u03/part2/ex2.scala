package it.unibo.pps.u03.part2

import u03.Sequences.Sequence.*
import u03.Sequences.Sequence

object ex2:

  def foldLeft[A,B](s : Sequence[A])(acc : B)(op : (B,A) => B) : B = (s,acc,op) match
    case (Nil(),acc,op) => acc
    case (Cons(h,t),acc,op) => foldLeft(t)(op(acc,h))(op)

  @main def ex2p2() =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    println(foldLeft(lst)(0)(_ - _))