package com.rockthejvm.part2effects

import cats.Traverse
import cats.effect.{IO, IOApp}

import scala.concurrent.Future
import scala.util.Random

object IOTraversal extends IOApp.Simple {

  import scala.concurrent.ExecutionContext.Implicits.global

  def heavyComputation(string: String): Future[Int] = Future {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }

  val workLoad: List[String] = List("I quite like CE", "Scala is great", "looking forward to some awesome stuff")
  def clunkyFutures(): Unit = {
    val futures: List[Future[Int]] = workLoad.map(heavyComputation)
    // Future[List[Int]] would be hard to obtain
    futures.foreach(_.foreach(println))
  }
  import cats.instances.list._
  val listTraverse = Traverse[List]
  def traverseFutures(): Unit = {
    // traverse
    val singleFuture: Future[List[Int]] = listTraverse.traverse(workLoad)(heavyComputation)
    // ^^ stores all the results in one future
    singleFuture.foreach(println)
  }

  import com.rockthejvm.utils._

  // traverse for IO
  def computeAsIO(string: String): IO[Int] = IO {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }.debug

  val ios: List[IO[Int]] = workLoad.map(computeAsIO)
  val singleIO: IO[List[Int]] = listTraverse.traverse(workLoad)(computeAsIO)

  // parallel traversal
  import cats.syntax.parallel._  // parTraverse extension method
  val parallelSingleIO: IO[List[Int]] = workLoad.parTraverse(computeAsIO)

  /**
   * Exercises
   */
  // hint: use the traverse API
  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] = listTraverse.traverse(listOfIOs)(identity)

  // hard version
  def sequence_v2[F[_] : Traverse, A](FofIOs: F[IO[A]]): IO[F[A]] = Traverse[F].traverse(FofIOs)(identity)

  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] = listOfIOs.parTraverse(identity)

  def parSequence_v2[F[_] : Traverse, A](FofIOs: F[IO[A]]): IO[F[A]] = FofIOs.parTraverse(identity)

  // existing sequence API
  val singleIO_v2: IO[List[Int]] = listTraverse.sequence(ios)

  // parallel sequencing
  val parallelSingleIO_v2: IO[List[Int]] = parSequence(ios) // from the exercise
  val parallelSingleIO_v3: IO[List[Int]] = ios.parSequence // extensionmethod from Parallel syntax package

  override def run: IO[Unit] = {
    //singleIO.map(_.sum).debug.void
    parallelSingleIO_v2.map(_.sum).debug.void

    //println(ios)
    //println(parSequence(ios))
    //IO.pure(())
  }
}
