package com.rockthejvm.part2effects

import cats.Parallel
import cats.effect.{IO, IOApp}

object IOParallelism extends IOApp.Simple{

  // IOs are usually sequential
  val anisIO = IO(s"[${Thread.currentThread().getName}] Ani")
  val kamranIO = IO(s"[${Thread.currentThread().getName}] Kamran")
  val composedIO = for {
    ani <- anisIO
    kamran <- kamranIO
  } yield s"$ani and $kamran love Rock the JVM"

  // debug extension method
  import com.rockthejvm.utils._
  // mapN extension method
  import cats.syntax.apply._
  val meaningOfLife: IO[Int] = IO.delay(42)
  val favoriteLanguage: IO[String] = IO.delay("Scala")
  val goalInLife = (meaningOfLife.debug, favoriteLanguage.debug).mapN((num, string) => s"My goal in life is $num and $string")

  // parallelism on IOs
  // convert a sequential IO to parallel IO
  val parIO1: IO.Par[Int] = Parallel[IO].parallel(meaningOfLife.debug)
  val parIO2: IO.Par[String] = Parallel[IO].parallel(favoriteLanguage.debug)
  import cats.effect.implicits._
  val goalInLifeParallel: IO.Par[String] = (parIO1, parIO2).mapN((num, string) => s"My goal in life is $num and $string")
  // turn back into a sequential
  val goalInLife_v2: IO[String] = Parallel[IO].sequential(goalInLifeParallel)

  // shorthand:
  import cats.syntax.parallel._
  val goalInLife_v3 = (meaningOfLife.debug, favoriteLanguage.debug).parMapN((num, string) => s"My goal in life is $num and $string")


  val aFailure: IO[String] = IO.raiseError(new RuntimeException("I can't do this!"))
  // compose success + failure
  val parallelWithFailure = (meaningOfLife.debug, aFailure.debug).parMapN(_ + _)
  //compose failure with failure
  val anotherFailure: IO[String] = IO.raiseError(new RuntimeException("Second failure!"))
  val twoFailures: IO[String] = (aFailure.debug, anotherFailure.debug).parMapN(_ + _)
  val twoFailuresDelayed: IO[String] = (IO(Thread.sleep(1000)) >> aFailure.debug, anotherFailure.debug).parMapN(_ + _)

  override def run: IO[Unit] =
//    composedIO.map(println)
//    goalInLife.map(println)
    //goalInLife_v2.debug.void //map(println)
    //goalInLife_v3.debug.void
    //parallelWithFailure.debug.void
    //twoFailures.debug.void
    twoFailuresDelayed.debug.void

}
