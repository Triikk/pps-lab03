package it.unibo.pps.u03.part3

import u03.Streams.Stream
import u03.Streams.Stream.*

object ex4 {

  @main def ex4p3 = {
    val s1 = cons(1, cons(3, cons(5, empty())))
    val s2 = cons(2, cons(4, cons(6, cons(8, cons(10, empty())))))
    println(Stream.toList(Stream.interleave(s1, s2)))
  }
}