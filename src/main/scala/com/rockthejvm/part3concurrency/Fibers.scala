package com.rockthejvm.part3concurrency

import cats.effect.kernel.Outcome.{Errored, Succeeded, Canceled}
import cats.effect.{Fiber, IO, IOApp, Outcome}
import com.rockthejvm.utils.*
import scala.concurrent.duration._

object Fibers extends IOApp.Simple {

  val meaningOfLife = IO.pure(42)
  val favLang = IO.pure("scala")

  def sameThreadIOs() = for {
    _ <- meaningOfLife.debug
    _ <- favLang.debug
  } yield ()

  // introduce the Fiber
  def createFiber: Fiber[IO, Throwable, String] = ??? // almost impossible to create fibers manually

  // the fiber is not actually started, but the fiber allocation is wrapped in another effect
  val aFiber: IO[Fiber[IO, Throwable, Int]] = meaningOfLife.debug.start

  def differentThreadIOs() = for {
    _ <- aFiber
    _ <- favLang.debug
  } yield ()

  // joining a fiber -- means that you wait for a fiber to finish (in a purely functional way)
  def runOnSomeOtherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- io.start
    result <- fib.join // an effect which waits for the fiber to terminate
  } yield result
  /*
    IO[ResultType of fib.join]
    fib.join = Outcome[IO, Throwable, A]

    possible outcomes:
      - success with an IO
      - failure with an exception
      - cancelled
  */

  val someIOonAnotherThread = runOnSomeOtherThread(meaningOfLife)
  val someResultFromAnotherThread = someIOonAnotherThread.flatMap {
    case Succeeded(effect) => effect
    case Errored(e) => IO(0)
    case Canceled() => IO(0)
  }

  def throwOnAnotherThread() = for {
    fib <- IO.raiseError[Int](new RuntimeException("no number available")).start
    result <- fib.join
  } yield result


  def testCancel() = {
    val task = IO("starting").debug >> /* IO(Thread.sleep(1000)) */ IO.sleep(1.second) >> IO("done").debug
    val taskWithCancellationHandler = task.onCancel(IO("I'm being cancelled").debug.void)
    // onCancel is a "finalizer", allowing you to free up resources in case you get cancelled
    for {
      fib <- taskWithCancellationHandler.start // on a separate thread
      _ <- IO.sleep(500.millis) >> IO("cancelling").debug
      _ <- fib.cancel
      result <- fib.join
    } yield result
  }

  /**
   * Exercises:
   *  1. Write a function that runs an IO on another thread, and, depending on the result of the fiber
   *    - return the result in an IO
   *    - if errored or cancelled, return a failed IO
   *
   *  2. Write a function that takes two IOs, runs them on different fibers and returns an IO with a tuple containing both results
   *    - if both IOs complete successfully, tuple their results
   *    - if the first IO returns an error, raise that erre (ignoring the second IO's result/error)
   *    - if the first IO doesn't error, but second IO returns an error, raise that error
   *    - if one (or both) cancelled, raise a RuntimeException
   *
   *  3. Write a function that adds a timeout to an IO:
   *    - IO runs on a fiber
   *    - if the timeout duration passes the fiber is canceled
   *    - the method returns an IO[A] which contains
   *      - the original value if the computation is successful before the timeout signal
   *      - the exception if the computation is failed before the timeout signal
   *      - a RuntimeException if it times out (i.e. cancelled by the timeout)
   */

  type FiberResult[A] = Outcome[IO, Throwable, A]

  // 1
  def processResultsFromFiber[A](io: IO[A]): IO[A] = {
    val res = for {
      fib <- io.debug.start
      result <- fib.join // an effect which waits for the fiber to terminate
    } yield result

    res.flatMap {
      case Succeeded(effect) => effect
      case Errored(e) => IO.raiseError(e)
      case Canceled() => IO.raiseError(new RuntimeException("fiber was cancelled"))
    }
  }

  def testEx1() = {
    val aComputation = IO("starting").debug >> IO.sleep(1.second) >> IO("Done!").debug >> IO(42)
    processResultsFromFiber(aComputation).void
  }

  // 2
  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = {
    val res: IO[(FiberResult[A], FiberResult[B])]  = for {
      fibA <- ioa.debug.start
      fibB <- iob.debug.start
      resultA <- fibA.join
      resultB <- fibB.join
    } yield (resultA, resultB)

    res.flatMap {
      case (Succeeded(fa), Succeeded(fb)) => fa.flatMap(a => fb.map(b => (a, b)))
      case (Errored(ea), _) => IO.raiseError(ea)
      case (_, Errored(eb)) => IO.raiseError(eb)
      case (_, _) => IO.raiseError(new RuntimeException("One or both of the fibers were cancelled"))
    }
  }

  def testEx2() = {
    val aComputation = IO("starting").debug >> IO.sleep(1.second) >> IO("Done!").debug >> IO(42)
    val bComputation = IO("I love programming").debug >> IO.sleep(2.second) >> IO("Done programming!").debug >> IO("It's Miller time")

    tupleIOs(aComputation, bComputation).debug.void
  }


  // 3
  def addTimeOut[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val taskWithCancellationHandler = io.onCancel(IO("I'm being cancelled before I finish").debug.void)
    // onCancel is a "finalizer", allowing you to free up resources in case you get cancelled
    val outcome = for {
      fib <- taskWithCancellationHandler.start // on a separate thread
      _ <- IO.sleep(duration) >> fib.cancel
      // _ <- (IO.sleep(duration) >> fib.cancel).start // would result in finishing when first thing completes, but this could cause the extra fiber to leak resources
      result <- fib.join
    } yield result

    outcome.flatMap {
      case Succeeded(fa) => fa
      case Errored(e) => IO.raiseError(e)
      case Canceled() => IO.raiseError(new RuntimeException("Ran out of time to complete task"))
    }
  }

  def testEx3() = {
    val aComputation: IO[Int] = IO("starting").debug >> IO.sleep(1.second) >> IO("Done!").debug >> IO(42)
    val timeout = 5000.millis
    addTimeOut(aComputation, timeout).debug.void
  }


  override def run = // differentThreadIOs() //sameThreadIOs()
//    runOnSomeOtherThread(meaningOfLife) // IO(Succeeded(IO(42)))
//      .debug.void
    //throwOnAnotherThread().debug.void
    //testCancel().debug.void
    //testEx1()
    //testEx2()
    testEx3()
}
