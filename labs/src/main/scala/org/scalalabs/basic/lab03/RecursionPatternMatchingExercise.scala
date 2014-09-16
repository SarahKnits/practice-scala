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
    case x::y::tail if x==y =>
      (groupConsecutive(in.slice(1,in.length))(0) :+ x) :: groupConsecutive(in.slice(1,in.length)).tail
    case x::y::tail =>
      List(x) :: groupConsecutive(in.slice(1,in.length))
    case x::tail => List(List(x))
    case _ => List(List.empty)
  }

  /**
   * Group Equal values
   * List(1,1,2,3,1,1) -> List(1,1,1,1), List(2), List(3)
   */
  def groupEquals[T](in: List[T]): List[List[T]] = {
    in match {
      case x::tail => addToProperListWrapper(groupEquals(in.tail), x)
      case _ => List(List.empty)
    }
  }

  /*
   * Helper function for groupEquals
   * Adds the current elements to the correct existing list or creates a new one
   */
  def addToProperList[T](in: List[List[T]], x:T) : List[List[T]] = {
    in match {
      case y::tail if y.length > 0 && (y(0) == x) => (x :: y) :: addToProperList(tail,x)
      case y::tail => y :: addToProperList(tail,x)
      case _ => List[List[T]]()
    }
  }

  /**
   * Wrapper for addToProperList
   * @param in List of lists to add x to
   * @param x element to be added
   * @tparam T Unknown type of awesomeness
   * @return in with x added in proper spot
   */
  def addToProperListWrapper[T](in: List[List[T]], x:T) : List[List[T]] = {
    val newList = addToProperList(in, x)
    newList match {
      case List(List()) => List(List(x))
      case y if y == in => y :+ List(x)
      case _ => newList
    }
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

  /**
   * Helper method for amountEqualMembers
   * Increases the count for the current x or adds it
   * @param in Current list of counts and items
   * @param x Current item to be added
   * @tparam T Unknown type
   * @return in with count increased for x
   */
  def addWithProperCounting[T](in: List[(Int, T)], x:T) : List[(Int, T)] = {
    var elem = in.find(t => t._2 == x)
    elem match {
      case None => Tuple2(1,x) :: in
      case _ => Tuple2(elem.get._1 + 1, x) :: in.filterNot(t => t == elem.get)
    }
  }
  
  /**
   * Zip multiple lists
   * List(List(1,2,3), List('A, 'B, 'C), List('a, 'b, 'c)) -> List(List(1, 'A, 'a), List(2, 'B, 'b), List(3, 'C, 'c))
   */
  def zipMultiple(in: List[List[_]]): List[List[_]] = {
    in match {
      case l::m::tail => addToList(l, zipMultiple(in.tail))
      case l::tail => initLists(l)
      case _ => List[List[_]]()
    }
  }

  /**
   * Helper method for zipMultiple
   * Adds elements from curList to existing totalList
   * @param curList List to add elements from
   * @param totalList Current product of recursive zipping
   * @return totalList with elements from curList added
   */
  def addToList(curList: List[_], totalList: List[List[_]]): List[List[_]] = {
    totalList match {
      case x::tail => (curList(0) :: x) :: addToList(curList.tail, tail)
      case _ => List[List[_]]()
    }
  }

  /**
   * Helper method for zipMultiple
   * Initializes x lists with the elements of l, where x is the length of l
   * @param l List to initialize
   * @return A list of lists
   */
  def initLists(l:List[_]): List[List[_]] = {
    l match {
      case x::tail => List(x) :: initLists(tail)
      case _ => List[List[_]]()
    }
  }

  /**
   * Zip multiple lists with different sizes
   * List(List(1), List('A, 'B, 'C), List('a, 'b)) -> List(List(1, 'A, 'a))
   */
  def zipMultipleWithDifferentSize(in: List[List[_]]): List[List[_]] = {
    zipMultipleWithDifferentSizeHelper(in,minLength(in,-1))
  }

  /**
   * Helper method for zipMultipleWithDifferentSize
   * Used to allow the minimum length of original lists to be used
   * @param in List of lists
   * @param min minimum length of existing list
   * @return List of lists
   */
  def zipMultipleWithDifferentSizeHelper(in: List[List[_]], min:Int): List[List[_]] = {
    in match {
      case l::m::tail => addToListLen(l, zipMultiple(in.tail),min)
      case l::tail => initListLen(l,min)
      case _ => List[List[_]]()
  }
  }

  /**
   * Helper method for zipMultipleWithDifferentSize
   * Recursively calculates minimum length of a list from in
   * @param in List of lists
   * @param curMin Current minimum length, or -1 if starting
   * @return minimum length of a list
   */
  def minLength(in: List[List[_]], curMin:Int): Int = {
    in match {
      case l::tail if curMin == -1 => minLength(in.tail, l.length)
      case l::tail if curMin > l.length => minLength(in.tail, l.length)
      case l::tail => minLength(in.tail, curMin)
      case _ => curMin
    }
  }

  /**
   * Helper method for zipMultipleWithDifferentSize
   * Initializes list of lists with first min elements of l
   * @param l List whose elements will make up new list
   * @param min Number of elements to include
   * @return List of Lists
   */
  def initListLen(l:List[_], min:Int): List[List[_]] = {
    l match {
      case x::tail if min > 0 => List(x) :: initListLen(tail, min-1)
      case _ => List[List[_]]()
    }
  }

  /**
   * Helper method for zipMultipleWithDifferentSize
   * Adds first min elements of curList to totalList
   * @param curList List to add elements from
   * @param totalList List of lists returned by recursive call
   * @param min Number of elements to add
   * @return totalList with elements from curList added
   */
  def addToListLen(curList: List[_], totalList: List[List[_]], min:Int): List[List[_]] = {
    totalList match {
      case x::tail if min > 0 => (curList(0) :: x) :: addToListLen(curList.tail, tail, min-1)
      case _ => List[List[_]]()
    }
  }

}
