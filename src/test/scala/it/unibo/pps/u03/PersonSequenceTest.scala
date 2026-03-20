package it.unibo.pps.u03

import it.unibo.pps.u03.PersonSequence.*
import it.unibo.pps.u03.PersonSequence.Person.*
import org.junit.*
import org.junit.Assert.*
import u03.Sequences.Sequence
import u03.Sequences.Sequence.{Cons, Nil}

class PersonSequenceTest {

  @Test def testTeacherCourses(): Unit = {
    val teacher = Teacher("Mario", "PPS")
    val student = Student("Luca", 2003)
    val persons: Sequence[Person] = Cons(student, Cons(teacher, Nil()))
    val rightCourses = Cons("PPS", Nil())
    assertEquals(rightCourses, getTeacherCourses(persons))
  }

  @Test def testFoldLeft(): Unit =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    val result = -16
    assertEquals(result, foldLeft(lst)(0)(_ - _))

  // use a combination of filter, map, distinct and foldLeft to compute the sum of the squares of the distinct even numbers in the list
  @Test def testGetDistinctCourses(): Unit =
    val viroli = Teacher("Viroli", "PPS")
    val aguzzi = Teacher("Aguzzi", "PPS")
    val ricci = Teacher("Ricci", "PCD")
    val teachers: Sequence[Person] = Cons(viroli, Cons(aguzzi, Cons(ricci, Nil())))
    assertEquals(2, getDistinctCourses(teachers))

  @Test def testGetDistinctCoursesWithStudents(): Unit =
    val viroli = Teacher("Viroli", "PPS")
    val aguzzi = Teacher("Aguzzi", "PPS")
    val ricci = Teacher("Ricci", "PCD")
    val nicola = Student("Nicola", 2003)
    val teachers: Sequence[Person] = Cons(viroli, Cons(aguzzi, Cons(ricci, Cons(nicola, Nil()))))
    assertEquals(2, getDistinctCourses(teachers))
}
