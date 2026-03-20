package it.unibo.pps.u03

import u03.Sequences.Sequence
import u03.Sequences.Sequence.*

import scala.annotation.tailrec

object PersonSequence {
  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:
    def name(p: Person): String = p match
      case Student(n, _) => n
      case Teacher(n, _) => n

    def course(p: Person): String = p match
      case Student(_, _) => null
      case Teacher(_, c) => c

    def isTeacher: Person => Boolean =
      case Person.Teacher(_, _) => true
      case _ => false
  import Person.*

  def getTeacherCourses(s: Sequence[Person]): Sequence[String] = s match
    case Nil() => Nil()
    case Cons(h, t) => h match
      case Student(_, _) => getTeacherCourses(t)
      case Teacher(_, c) => Cons(c, getTeacherCourses(t))

  @tailrec
  def foldLeft[T](s: Sequence[T])(acc: T)(f: (a: T, b: T) => T): T = s match
    case Nil() => acc
    case Cons(h, t) => foldLeft(t)(f(acc, h))(f)

  def getDistinctCourses(s: Sequence[Person]): Int = {
    foldLeft(map(distinct(map(filter(s)(isTeacher))(course)))(d => 1))(0)(_ + _)
  }
}
