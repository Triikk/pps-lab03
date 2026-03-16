package it.unibo.pps.u03.part3
import u03.Streams.Stream

object ex1:



  @main def exe1p3() =
    val stream = Stream.iterate(0)(_ + 1)
    println(Stream.toList(Stream.takeWhile(stream)(_ < 5)))