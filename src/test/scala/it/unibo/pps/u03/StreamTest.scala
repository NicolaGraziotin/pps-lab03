package it.unibo.pps.u03

import org.junit.*
import org.junit.Assert.*
import u03.Streams.Stream
import u03.Sequences.Sequence
import u03.Sequences.Sequence.*

class StreamTest {

  @Test def testTakeWhile(): Unit = {
    val stream: Stream[Int] = Stream.iterate(0)(_ + 1)
    val actual: Sequence[Int] = Stream.toList(Stream.takeWhile(stream)(_ < 5))
    val expected: Sequence[Int] = Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil())))))
    assertEquals(expected, actual)
  }

  @Test def testFill(): Unit = {
    val actual: Sequence[String] = Stream.toList(Stream.fill(3)("a"))
    val expected: Sequence[String] = Cons("a", Cons("a", Cons("a", Nil())))
    assertEquals(expected, actual)
  }
  
  @Test def testFibonacci(): Unit = {
    val fibonacci: Stream[Int] = Stream.fibonacci
    val actual: Sequence[Int] = Stream.toList(Stream.take(fibonacci)(5))
    val expected: Sequence[Int] = Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Nil())))))
    assertEquals(expected, actual)
  }

  @Test def testInterleave(): Unit = {
    val s1: Stream[Int] = Stream.fromList(List(1, 3, 5))
    val s2: Stream[Int] = Stream.fromList(List(2, 4, 6, 8, 10))
    val actual: Sequence[Int] = Stream.toList(Stream.interleave(s1, s2))
    val expected: Sequence[Int] = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(8, Cons(10, Nil()))))))))
    assertEquals(expected, actual)
  }

  @Test def testcycle(): Unit = {
    val repeat: Stream[Char] = Stream.cycle(Cons('a', Cons('b', Cons('c', Nil()))))
    val actual: Sequence[Char] = Stream.toList(Stream.take(repeat)(5))
    val expected: Sequence[Char] = Cons('a', Cons('b', Cons('c', Cons('a', Cons('b', Nil())))))
    assertEquals(expected, actual)
  }
}
