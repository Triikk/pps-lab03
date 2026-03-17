package it.unibo.pps.u03.part2
import u03.Sequences.Sequence
import u03.Sequences.Sequence.*
import it.unibo.pps.u02.Person.*
import it.unibo.pps.u02.Person
import it.unibo.pps.u02.Person.Person.{Student, Teacher}

object ex3:

  def getTeachersCourses(s: Sequence[Person]): Int = sum(map(distinct(flatMap(s)(t => t match
    case Student(_,_) => Nil()
    case Teacher(_,course) => Cons(course,Nil())
  )))(c => 1))

  @main def ex3p2() =
    val teachers = Cons(Teacher("Mirko Viroli", "PPS"), Cons(Teacher("Alessandro Ricci", "PPD"), Cons(Teacher("Aguzzi","PPS"),Nil())))
    println(getTeachersCourses(teachers))
