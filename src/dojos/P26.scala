package dojos

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object P26 {

  def combinations[T: ClassManifest](k: Int, ls: List[T]): List[List[T]] = {

    if (k < 1) return Nil
    if (k == 1) return ls.map(x => List(x))

    val labels = ls.toArray
    val n = labels.length

    if (k > n) return Nil

    val indices = Array.range(n - k + 1, n + 1)
    val maxIndices = indices.clone

    @tailrec
    def findIndexFrom(i: Int): Int =
      if (i < 0 || indices(i) != maxIndices(i)) i
      else findIndexFrom(i - 1)

    @inline
    def findIndex(): Int = findIndexFrom(k - 1)

    @inline
    def combine(i: Int): Array[Int] = {
      indices(i) += 1
      for (x <- i + 1 until k) indices(x) = indices(x - 1) + 1
      indices
    }

    @inline
    def map(c: Array[Int]): List[T] =
      c.map(x => labels(x - 1)).toList

    @tailrec
    def combinations(buffer: ListBuffer[List[T]], i: Int): List[List[T]] =
      if (i < 0) buffer.toList
      else combinations(buffer += map(combine(i)), findIndex)

    indices(0) = 0
    combinations(new ListBuffer[List[T]], 0)

  }

  def main(args: Array[String]) {

    val input = "ABCDE".toList
    println(input)
    println

    combinations(-1, input).foreach(println)
    println

    combinations(0, input).foreach(println)
    println

    combinations(1, input).foreach(println)
    println

    combinations(2, input).foreach(println)
    println

    combinations(3, input).foreach(println)
    println

    combinations(4, input).foreach(println)
    println

    combinations(5, input).foreach(println)
    println

    combinations(1, Nil).foreach(println)
    println

    //benchmarks

  }

  ////////////////////////////////////////////////////////////////////////

  /**
   * code from: http://aperiodic.net/phil/scala/s-99/p26.scala
   */
  def flatMapSublists[A, B](ls: List[A])(f: (List[A]) => List[B]): List[B] = {
    ls match {
      case Nil => Nil
      case sublist @ (_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }
  }
  def combinations2[A](n: Int, ls: List[A]): List[List[A]] = {
    if (n == 0) List(Nil)
    else flatMapSublists(ls) { sl =>
      combinations2(n - 1, sl.tail) map { sl.head :: _ }
    }
  }

  ////////////////////////////////////////////////////////////////////////  

  def time[T](f: => T): Long = {
    val t0 = System.currentTimeMillis
    try {
      f
      val t1 = System.currentTimeMillis
      return t1 - t0;
    } catch {
      case ex: StackOverflowError => 10000000000L
      case ex: OutOfMemoryError => 10000000000L
    }
  }

  def getMinSumMax[T](f: => T, tuple: (Long, Long, Long)): (Long, Long, Long) = {
    var min = tuple._1
    var max = tuple._3
    val t = time(f)
    if (t < min) min = t
    if (t > max) max = t
    (min, (tuple._2 + t) / 2, max)
  }

  def benchmark[T](f: => T, times: Int): (Long, Long, Long) = {
    var tuple = (10000000000L, 0L, 0L)
    for (_ <- 1 to times) tuple = getMinSumMax(f, tuple)
    tuple
  }

  ////////////////////////////////////////////////////////////////////////

  def benchmarks(): Unit = {

    val input = "ABCDEFGHIJKLMNOPQRSTUV".toList

    println(input)
    println

    val time = 10
    
    println(benchmark(combinations2(-1, input), time))
    println(benchmark(combinations(-1, input), time))
    println    
    
    println(benchmark(combinations2(0, input), time))
    println(benchmark(combinations(0, input), time))
    println
    
    println(benchmark(combinations2(1, input), time))
    println(benchmark(combinations(1, input), time))
    println        

    println(benchmark(combinations2(4, input), time))
    println(benchmark(combinations(4, input), time))
    println

    println(benchmark(combinations2(5, input), time))
    println(benchmark(combinations(5, input), time))
    println

    println(benchmark(combinations2(6, input), time))
    println(benchmark(combinations(6, input), time))
    println

    println(benchmark(combinations2(7, input), time))
    println(benchmark(combinations(7, input), time))
    println

    println(benchmark(combinations2(8, input), time))
    println(benchmark(combinations(8, input), time))
    println
    
    println(benchmark(combinations2(18, Nil), time))
    println(benchmark(combinations(18, Nil), time))
    println    

    println(benchmark(combinations2(18, input), time))
    println(benchmark(combinations(18, input), time))
    println

  }

}