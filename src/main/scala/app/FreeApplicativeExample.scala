package app

import cats.instances.vector._
import cats.syntax.traverse._
import cats.syntax.cartesian._
import demo.Effects.S._
import demo.Effects.{AppActionApplicative, AppActionMonadic, noAction}
import model.{Handle, Tweet}
import cats.free.FreeApplicative
import cats.free.FreeApplicative.lift
import FreeMonadExample.calculateActiveCount

import scala.concurrent.duration._

object FreeApplicativeExample {

  def findMostInfluentialAccount(handle1: Handle, handle2: Handle): AppActionMonadic[Handle] = {
    for {
      handleFollowers <- noAction(getHandlesInParallel(handle1, handle2))
      handleActiveFollowers <- noAction(getFollowersInParallel(handleFollowers._1, handleFollowers._2))
      mostActive = if (handleActiveFollowers._1 > handleActiveFollowers._2) handle1 else handle2
    } yield mostActive
  }

  private def getHandlesInParallel(handle1: Handle, handle2: Handle): AppActionApplicative[(Vector[Handle], Vector[Handle])] = {
    (getFollowersA(handle1) |@| getFollowersA(handle2)).map{ (a1, b1) => (a1, b1) }
  }

  private def getFollowersInParallel(handle1Followers: Vector[Handle], handle2Followers: Vector[Handle]): AppActionApplicative[(Int, Int)] = {
    (getUsersActiveTweetCountA(handle1Followers) |@| getUsersActiveTweetCountA(handle2Followers)).map { (a1, b1) => (a1, b1) }
  }


  def getUsersActiveTweetCountA(users: Vector[Handle]): AppActionApplicative[Int] = {
    val result: AppActionApplicative[Int] = users.traverse[AppActionApplicative, Tweet] { handle =>
      getMostRecentTweetA(handle)
    }.map(calculateActiveCount)

    result
  }
}

