package it.unibo.pps.u03.part3

import u03.Streams.Stream
import u03.Streams.Stream.*

object ex3:

  def fibonacciStream(first : () => Int, second : () => Int) : Stream[Int] =
    cons(first(), fibonacciStream(second, () => first()+second()))

  @main def ex3p3() =
    val fibonacci : Stream[Int] = fibonacciStream(() => 0, () => 1)
    println(Stream.toList(Stream.take(fibonacci)(5)))