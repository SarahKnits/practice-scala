package org.scalalabs.basic.lab03
import sys._
/**
 * This exercise introduces you to pattern matching in combination with recursion.
 *
 * Recursion is a key concept for the functional style programming.
 * In the exercises below you learn how to apply recursion in combination with Scala's pattern matching facilities.
 *
 * For this exercise exclusively use pattern matching constructs in order to make the corresponding unittest work.
 *
 * Reference material to solve these exercises can be found here:
 * Pattern matching in general: http://programming-scala.labs.oreilly.com/ch03.html#PatternMatching
 * Pattern matching and recursion: http://programming-scala.labs.oreilly.com/ch08.html#Recursion
 */

object RecursionPatternMatchingExercise {

  /**
   * ***********************************************************************
   * Recursive algorithms with pattern matching
   * For expected solution see unittest @RecursionPatternMatchingExerciseTest
   * ***********************************************************************
   */
  /**
   * Create a method that checks that each subsequent value is greater than
   * the previous one.
   * E.g.:
   * checkValuesIncrease(Seq(1,2,3)) == true
   * checkValuesIncrease(Seq(1,2,2)) == false
   */
  def checkValuesIncrease(seq: Seq[Int]): Boolean = {
    seq match {
      case x::y::tail => if (x < y) checkValuesIncrease(seq.slice(1,seq.size)) else false
      case _ => true
    }
  }
  
  /**
   * Group Consecutive values
   * List(1,1,2,3,1,1) -> List(1,1), List(2), List(3), List(1,1)
   */
  def groupConsecutive[T](in: List[T]): List[List[T]] = in match {
    case x::y::tail => {
      if (x==y) {
        ((groupConsecutive(in.slice(1,in.length)))(0) :+ x) :: groupConsecutive(in.slice(1,in.length)).tail
      } else {
      List(x) :: groupConsecutive(in.slice(1,in.length))
      }
    }
    case x::tail => List(List(x))
    case _ => List(List.empty)
  }

  /**
   * Group Equal values
   * List(1,1,2,3,1,1) -> List(1,1,1,1), List(2), List(3)
   */
  def groupEquals[T](in: List[T]): List[List[T]] = {
    in match {
      case x::tail => addToProperList(groupEquals(in.tail), x)
      case _ => List(List.empty)
    }
  }

  def addToProperList[T](in: List[List[T]], x:T) : List[List[T]] = {
    var newList = in.map(l => if (l.length > 0 && l(0)==x) l :+ x else l)
    if (newList == List(List())) List(List(x)) else if (newList == in) newList :+ List(x) else newList
  }

  /**
   * Compress values
   * List(1,1,2,3,1,1) -> List(1,2,3)
   */
  def compress[T](in: List[T]): List[T] = {
    in match {
      case x::tail => if (compress(in.tail).count(i => i == x) == 0) x :: compress(in.tail) else compress(in.tail)
      case _ => List()
    }
  }
  
  /**
   * Define the amount of all equal members
   * List(1,1,2,3,1,1) -> List((4,1),(1,2),(1,3))
   */
  def amountEqualMembers[T](in: List[T]): List[(Int, T)] = {
    in match {
      case x::tail => addWithProperCounting(amountEqualMembers(in.tail), x)
      case _ => List()
    }
  }

  def addWithProperCounting[T](in: List[(Int, T)], x:T) : List[(Int, T)] = {
    var elem = in.find(t => t._2 == x)
    if (elem == None) Tuple2(1,x) :: in else Tuple2(elem.get._1 + 1, x) :: in.filterNot(t => t == elem.get)
  }
  
  /**
   * Zip multiple lists
   * List(List(1,2,3), List('A, 'B, 'C), List('a, 'b, 'c)) -> List(List(1, 'A, 'a), List(2, 'B, 'b), List(3, 'C, 'c))
   */
  def zipMultiple(in: List[List[_]]): List[List[_]] = {
    in match {
      case l::tail => helperMaster(l, zipMultiple(in.tail))
      case _ => List(List())
    }
  }

  def helperMaster(frank: List[_], jerry: List[List[_]]): List[List[_]] = {
    var i = 0
    var newJerry = List[List[_]]()
    for(i <- 1 to frank.length) {
      newJerry = newJerry :+ (frank(i-1) :: jerry(i-1))
    }
    newJerry
  }

  /**
   * Zip multiple lists with different sizes
   * List(List(1), List('A, 'B, 'C), List('a, 'b)) -> List(List(1, 'A, 'a))
   */
  def zipMultipleWithDifferentSize(in: List[List[_]]): List[List[_]] = {
    error("fix me")
  }

}
