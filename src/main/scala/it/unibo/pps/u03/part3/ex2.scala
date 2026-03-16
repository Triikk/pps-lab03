package it.unibo.pps.u03.part3

import u03.Streams.Stream

object ex2 {

  @main def ex2p3() =
    println(Stream.toList(Stream.fill(3)("a")))
}
