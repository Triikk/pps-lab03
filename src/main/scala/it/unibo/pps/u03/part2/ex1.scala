package it.unibo.pps.u03.part2
import u03.Sequences.Sequence
import u03.Sequences.Sequence.*
import it.unibo.pps.u02.Person.*
import it.unibo.pps.u02.Person
import it.unibo.pps.u02.Person.Person.{Student, Teacher}

object ex1:

  def getTeachersCourses(s : Sequence[Person]) : Sequence[String] = map(filter(s)(p => p match
    case p if Person.Person.isStudent(p) => false
    case _ => true
  ))(t => t match
    case Teacher(name,course) => course
    case _ => ""
  )

  def getTeachersCoursesCleaner(s: Sequence[Person]): Sequence[String] = flatMap(s)(t => t match
      case Student(_,_) => Nil()
      case Teacher(_,course) => Cons(course,Nil())
  )

  @main def ex1p2() =
    val teachers = Cons(Teacher("Mirko Viroli", "PPS"), Cons(Teacher("Alessandro Ricci", "PPD"), Nil()))
    println(getTeachersCourses(teachers))
    println(getTeachersCoursesCleaner(teachers))
