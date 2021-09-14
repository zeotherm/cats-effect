package com.rockthejvm.part2effects

import scala.concurrent.Future
import scala.io.StdIn.readLine

object Effects {
  // pure functional programming
  // substitution - replace and expression with the value it evaluates to
  def combine(a: Int, b: Int): Int = a + b
  val five = combine(2, 3)
  val five_v2 = 2 + 3
  val five_v3 = 5

  // referential transparency = can replace an expression with
  // its value as many time as we want without changing behavior

  // example: print to the console -- famous example of a side effect
  val printSomething: Unit = println("Cats Effect")
  val printSomething_v2: Unit = () // not identical

  // example: change a variable
  var anInt = 0
  val changingVar: Unit = (anInt += 1) // unit signifies the ACT of changing, not the EFFECT of changing
  val changingVar_v2: Unit = () // not the same thing, could not be swapped in for previous line

  // side effect are inevitable for useful programs

  // effect - data type that embodies a side effect
  /*
    Effect types
    Properties:
      - type signature describes the kind of calculation that will be performed
      - type signature describes the VALUE that will be calculated
      - when side effects are needed, effect construction must be separated from effect excecution
  */

  /*
   example: Option is an effect type
      - describes a possibility of absense of a value
      - computes a value of type A, if it exists
      - side effects are NOT needed to construct and Option
   */
  val anOption: Option[Int] = Option(42)

  /*
    example: Future is NOT an effect type
      - describes an asynchronous computation
      - computes a value of type A, if it's successful
      - side effect is required (allocating/scheduling a thread), execution is NOT separate from construction
  */
  import scala.concurrent.ExecutionContext.Implicits.global
  val aFuture: Future[Int] = Future(42)

  //
  /*
    example: MyIO datatype from the Monads Lecture - it IS an effect type
      - describes any computation that might produce side effects
      - calculates a value of type A if it is successful
      - side effects are required for the evaluation of () => A
        + YES, the creation of MyIO does NOT produce side effects on construction
  */
  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())
  }

  val anIO: MyIO[Int] = MyIO(() => {
    println("I'm writing something...")
    42
  })

  /**
   *  Exercises
   *  1. An IO which returns the current time of the system (system.currenttime.millis)
   *  2. An IO which measure the duration of a computation (hint: use exercise #1)
   *  3. An IO which prints something to the console
   *  4. An IO which reads something from the console (line as a string)
   */

    // 1
    val currTimeIO: MyIO[Long] = MyIO(() => System.currentTimeMillis())

    // 2
    def measure[A](computation: MyIO[A]): MyIO[Long] = {
      for {
        start <- currTimeIO
        _ <- computation
        end <- currTimeIO
      } yield (end - start)
    }

    /*
      Deconstrcuting the for comprehension:
      currTimeIO.flatMap(start => computation.flatMap(_ => currTimeIO.map(end => end - start)))

      currTimeIO.map(end => end - start) = MyIO(() => clock.unsafeRun() - start))
      currTimeIO.map(end => end - start) = MyIO(() => System.currentTimeMillis() - start))
      => currTimeIO.flatMap(start => computation.flatMap(_ => MyIO(() => System.currentTimeMillis() - start)))

      computation.flatMap(lambda) = MyIO(() => lambda(computation.unsafeRun())
      computation.flatMap(lambda) = MyIO(() => lambda(___COMP___).unsafeRun())
                                  = MyIO(() => MyIO(() => System.currentTimeMillis() - start)).unsafeRun())
                                  = MyIO(() => System.currentTimeMillis_afterComp() - start)
      => clock.flatMap(startTime => MyIO(() => System.currentTimeMillis_afterComp() - start)))
        MyIO(() => lambda(clock.unsafeRun()).unsafeRun()))
      = MyIO(() => lambda(System.currentTimeMillis).unsafeRun())
      = MyIO(() => MyIO(() => System.currentTimeMillis_afterComp() - start)).unsafeRun())
      = MyIO(() =>  System.currentTimeMillis_afterComp() - System.currentTimeMillis_before())
    */

    // 3
    def printIO(s: String): MyIO[Unit] =
      MyIO(() => {
        println(s)
      })

    // 4
    def readIO(): MyIO[String] =
      MyIO(() => {
        println("Please enter something: ")
        readLine()
      })

  def testConsole(): Unit = {
    val program: MyIO[Unit] = for {
      line1 <- readIO()
      line2 <- readIO()
      _ <- printIO(line1 + line2)
    } yield ()

    println(program.unsafeRun())
  }

  def main(args: Array[String]): Unit = {
    anIO.unsafeRun()
    currTimeIO.unsafeRun()
    currTimeIO.unsafeRun()
    val longComp = MyIO(() => (1 to 100000).toList.sum)
    println(measure(longComp).unsafeRun())
    val reallyLongComp = MyIO(() => (1 to 500000).toList.sum)
    val reallyReallyLongComp = MyIO(() => (1 to 5000000).toList.sum)
    println(measure(reallyLongComp).unsafeRun())
    println(measure(reallyReallyLongComp).unsafeRun())
    val pIO = printIO("What...?")
    pIO.unsafeRun()

    //val rIO = readIO().unsafeRun()
    //println(s"The returned value was $rIO")

    testConsole()
  }
}
