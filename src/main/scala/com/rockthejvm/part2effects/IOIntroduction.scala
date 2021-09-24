package com.rockthejvm.part2effects

import cats.effect.IO

import scala.io.StdIn

object IOIntroduction {

  // IO - describes a datatype that may perform side effects
  val outFirstIO: IO[Int] = IO.pure(42) // arg should not have side effects,  pure is evaluated eagerly
  val aDelayedIO: IO[Int] = IO.delay {
    println("I am producing an integer")
    54
  }

//  val shouldNotDoThis: IO[Int] = IO.pure({
//    println("I am producing an integer")
//    54
//  })

  val aDelayedIO_v2: IO[Int] = IO {  // apply == delay
    println("I am producing an integer")
    54
  }

  // goal of cats effect is to compose transformations of IO datatypes
  // map, flatMap
  val improvedMeaningOfLife = outFirstIO.map(_ * 2)
  val printedMeaningOfLife = outFirstIO.flatMap(mol => IO.delay(println(mol)))

  def smallProgram(): IO[Unit] = for {
    line1 <- IO(StdIn.readLine())
    line2 <- IO(StdIn.readLine())
    _ <- IO.delay(println(line1 + line2))
  } yield ()

  // mapN - combine IO effects as tuples
  import cats.syntax.apply.*
  val combinedMeaningOfLife = (outFirstIO, improvedMeaningOfLife).mapN(_ + _)  // took two IOs and turned them into a single one
  def smallProgram_v2(): IO[Unit] =
    (IO(StdIn.readLine()), IO(StdIn.readLine())).mapN(_ + _).map(println)

  /**
   * Exercises
   */

  // 1 - sequence two IOs and take the result of the LAST one
  // hint: use flatMap
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa.flatMap(_ => iob)

  def sequenceTakeLast_v2[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa *> iob // "andThen"

  def sequenceTakeLast_v3[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa >> iob // "andThen" with by-name call

  // 2 - sequence two IOs and take the result of the FIRST one
  // hint: use flatMap
  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa.flatMap(a => iob.map(_ => a))

  def sequenceTakeFirst_v2[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa <* iob

  // 3 - repeat an IO forever
  // hint - use flatMap + recursion
  def forever[A](ioa: IO[A]): IO[A] =
    ioa.flatMap(_ => forever(ioa))

  def forever_v2[A](io: IO[A]): IO[A] =
    io >> forever_v2(io)  // same thing

  def forever_v3[A](io: IO[A]): IO[A] =
    io *> forever_v3(io)  // same thing

  def forever_v4[A](io: IO[A]): IO[A] =
    io.foreverM // with tail recursion

  // 4 - convert an IO to a different type
  // hint - use map
  def convert[A, B](io: IO[A], value: B): IO[B] =
    io.map(_ => value)

  def convert_v2[A, B](io: IO[A], value: B): IO[B] =
    io.as(value)


  // 5 - discard a value inside an IO, just return Unit
  def asUnit[A](ioa: IO[A]): IO[Unit] =
    ioa.flatMap(_ => IO.pure(()))

  def asUnit_a[A](ioa: IO[A]): IO[Unit] =
    ioa.map(_ => ())

  def asUnit_v2[A](ioa: IO[A]): IO[Unit] =
    ioa.as(()) // discourage - don't use this

  def asUnit_v3[A](ioa: IO[A]): IO[Unit] =
    ioa.void // same - encoraged


  //
//    for {
//      _ <- ioa
//    } yield ()

  // 6 - fix stack recursion
  def sum(n: Int): Int =
    if (n <= 0) 0
    else n + sum(n - 1)

  def sumIO(n: Int): IO[Int] =
    if (n <= 0) IO.pure(0)
    else for {
      lastNumber <- IO(n)
      prevSum <- sumIO(n - 1)
    } yield lastNumber + prevSum

  //(IO.pure(n)).flatMap(num => sumIO(n-1).flatMap(i => i + n))

  // 7 (hard) - write a fibonnaci IO function that does NOT crash on recursion
  // hints: use recursion, ignore expontntial time complexity, use flatMap heavily
  def fibonacci(n: Int): IO[BigInt] =
    if (n < 2) IO(1)
    else for {
      last <- IO(fibonacci(n-1)).flatten  // IO[IO[BigInt]], need to flatten
      prev <- IO.defer(fibonacci(n-2))  // same as .delay(...).flatten     .flatMap(x => x)
    } yield last + prev

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global  // "platform"
    // the "end of the world"
    //println(smallProgram().unsafeRunSync())
    //println(smallProgram_v2().unsafeRunSync())

    //forever(IO(println("Forever"))).unsafeRunSync()

//    forever_v2( IO {
//      println("forever v2!")
//      Thread.sleep(100)
//    }).unsafeRunSync()
//    forever_v3( IO {
//      println("forever v3!")
//      Thread.sleep(100)
//    }).unsafeRunSync()
//    println(sumIO(20000).unsafeRunSync())
//    println(sum(20000))
    println(fibonacci(10).unsafeRunSync())
    (1 to 100) foreach { i => println(fibonacci(i).unsafeRunSync())}
  }
}
