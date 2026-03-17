package u03

import u03.Optionals.Optional
import u03.Optionals.Optional.Just
import u03.Optionals.Optional.Empty

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t)            => filter(t)(pred)
      case Nil()                 => Nil()

    // Lab 03

    /*
     * Skip the first n elements of the sequence
     * E.g., [10, 20, 30], 2 => [30]
     * E.g., [10, 20, 30], 3 => []
     * E.g., [10, 20, 30], 0 => [10, 20, 30]
     * E.g., [], 2 => []
     */
    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = (s,n) match
      case (Nil(),n) => Nil()
      case (s,0) => s
      case (Cons(head,tail),n) => skip(tail)(n-1)

    /*
     * Zip two sequences
     * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
     * E.g., [10], [] => []
     * E.g., [], [] => []
     */
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first,second) match
      case (Nil(),second) => Nil()
      case (first,Nil()) => Nil()
      case (Cons(headFirst, tailFirst), Cons(headSecond, tailSecond)) => Cons((headFirst,headSecond),zip(tailFirst,tailSecond))

    /*
     * Concatenate two sequences
     * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
     * E.g., [10], [] => [10]
     * E.g., [], [] => []
     */
    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = (s1,s2) match
      case (Nil(),s2) => s2
      case (s1,Nil()) => s1
      case (Cons(head1,tail1),_) => Cons(head1,concat(tail1,s2))

    /*
     * Reverse the sequence
     * E.g., [10, 20, 30] => [30, 20, 10]
     * E.g., [10] => [10]
     * E.g., [] => []
     */
    def reverse[A](s: Sequence[A]): Sequence[A] =
      def reverseRec(s : Sequence[A], acc : Sequence[A]) : Sequence[A] = (s,acc) match
        case (Nil(),acc) => acc
        case (Cons(h,t),acc) => reverseRec(t, Cons(h,acc))
      reverseRec(s,Nil())

    /*
     * Map the elements of the sequence to a new sequence and flatten the result
     * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
     * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
     * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
     */
      // da guardare senza concat
    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = (s, mapper) match
      case (Nil(), mapper) => Nil()
      case (Cons(h, t), mapper) => concat(mapper(h), flatMap(t)(mapper))

    /*
     * Get the minimum element in the sequence
     * E.g., [30, 20, 10] => 10
     * E.g., [10, 1, 30] => 1
     */
    def min(s: Sequence[Int]): Optional[Int] =
      @annotation.tailrec
        def minRec(s : Sequence[Int], min : Optional[Int]) : Optional[Int] = (s,min) match {
        case (Nil(),min) => min
        case (Cons(h,t),Empty()) => minRec(t,Just(h))
        case (Cons(h,t),Just(x)) if h < x => minRec(t,Just(h))
        case (Cons(h,t),min) => minRec(t,min)
      }
      minRec(s,Empty())


    /*
     * Get the elements at even indices
     * E.g., [10, 20, 30] => [10, 30]
     * E.g., [10, 20, 30, 40] => [10, 30]
     */
    def evenIndices[A](s: Sequence[A]): Sequence[A] = s match {
      case Nil() => Nil()
      case Cons(h, Nil()) => Cons(h, Nil())
      case Cons(h1, Cons(h2, t2)) => Cons(h1, evenIndices(t2))
    }

    /*
     * Check if the sequence contains the element
     * E.g., [10, 20, 30] => true if elem is 20
     * E.g., [10, 20, 30] => false if elem is 40
     */
    def contains[A](s: Sequence[A])(elem: A): Boolean = (s,elem) match {
      case (Nil(),_) => false
      case (Cons(h,t),elem) if h == elem => true
      case (Cons(h,t),elem) => contains(t)(elem)
    }

    /*
     * Remove duplicates from the sequence
     * E.g., [10, 20, 10, 30] => [10, 20, 30]
     * E.g., [10, 20, 30] => [10, 20, 30]
     */
//    def distinct[A](s: Sequence[A]): Sequence[A] =
//    // quadratic
//      @annotation.tailrec
//        def isPresent(s : Sequence[A], elem : A) : Boolean = (s,elem) match {
//        case (Nil(),_) => false
//        case (Cons(h,t),elem) if h == elem => true
//        case (Cons(h,t),elem) => isPresent(t,elem)
//      }
//      @annotation.tailrec
//      def distinctRec(s : Sequence[A], acc : Sequence[A]) : Sequence[A]  = (s,acc) match {
//        case (Nil(),acc) => acc
//        case (Cons(h,t),acc) if !isPresent(acc,h) => distinctRec(t,concat(acc,Cons(h,Nil())))
//        case (Cons(h,t),acc) => distinctRec(t,acc)
//      }
//      distinctRec(s,Nil())

    def distinct[A](s: Sequence[A]): Sequence[A] =
      @annotation.tailrec
      def distinctRec(s: Sequence[A], acc: Sequence[A]): Sequence[A] = (s, acc) match {
        case (Nil(), acc) => acc
        case (Cons(h,t), acc) => distinctRec(filter(t)(_ != h), concat(acc,Cons(h,Nil())))
      }
      distinctRec(s, Nil())

    /*
     * Group contiguous elements in the sequence
     * E.g., [10, 10, 20, 30] => [[10, 10], [20], [30]]
     * E.g., [10, 20, 30] => [[10], [20], [30]]
     * E.g., [10, 20, 20, 30] => [[10], [20, 20], [30]]
     */
    def group[A](s: Sequence[A]): Sequence[Sequence[A]] =
      @annotation.tailrec
      def groupRec(s : Sequence[A], acc : Sequence[Sequence[A]]) : Sequence[Sequence[A]] = (s,acc) match {
        case (Nil(),acc) => reverse(acc)
        case (Cons(h,t),Nil()) => groupRec(t,Cons(Cons(h,Nil()),Nil()))
        case (Cons(h,t),Cons(Cons(h2,t2),st)) if h == h2 => groupRec(t,Cons(Cons(h2,concat(t2,Cons(h,Nil()))),st))
        case (Cons(h,t),Cons(sh,st)) => groupRec(t,Cons(Cons(h,Nil()),Cons(sh,st)))
      }
      groupRec(s,Nil())

    /*
     * Partition the sequence into two sequences based on the predicate
     * E.g., [10, 20, 30] => ([10], [20, 30]) if pred is (_ < 20)
     * E.g., [11, 20, 31] => ([20], [11, 31]) if pred is (_ % 2 == 0)
     */
    def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) =
      @annotation.tailrec
      def partitionRec(s : Sequence[A])(pred : A => Boolean)(acc : (Sequence[A], Sequence[A])): (Sequence[A], Sequence[A]) = (s,pred,acc) match {
        case (Nil(),pred,acc) => acc
        case (Cons(h,t),pred,(st,sf)) if pred(h) => partitionRec(t)(pred)((concat(st,Cons(h,Nil())),sf))
        case (Cons(h,t),pred,(st,sf)) => partitionRec(t)(pred)((st,concat(sf,Cons(h,Nil()))))
      }
      partitionRec(s)(pred)((Nil(),Nil()))

@main def trySequences =
  import Sequences.* 
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
