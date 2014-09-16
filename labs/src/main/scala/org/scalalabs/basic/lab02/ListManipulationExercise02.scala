package org.scalalabs.basic.lab02

import scala.collection.mutable.ListBuffer
 import sys._


object ListManipulationExercise02 {

  /**
   * Find the maximum element in a list, e.g. maxElementInList(List(1,9,3,5)) == 9
   * As usual, various ways exist: pattern matching, folding, ...
   */
  def maxElementInList(l: List[Int]): Int = l.foldLeft(0) {(x:Int, y:Int) => if (x>y) x else y}

  /**
   * Calculate the sum of the equally positioned elements
   * of the two list
   */
  def sumOfTwo(l1: List[Int], l2: List[Int]): List[Int] = {
    if (l1 == List.empty) l2 else if (l2 == List.empty) l1 else
    l1.zip(l2).map{case (x1, x2) => x1 + x2}
  }

  /**
   * Calculate the sum of the equally positioned elements
   * of an arbitrary number of lists
   */
  def sumOfMany(l: List[Int]*): List[Int] = l.fold(List()) {(x:List[Int], y:List[Int]) => sumOfTwo(x,y)}

  case class Person(age: Int, firstName: String, lastName: String)

  /**
   * The following method is implemented in the most in-elegant way we could think of.
   * The idea is to re-write the method into more functional style. In the end, you
   * may be able to achieve the same functionality as implemented below
   * in a one-liner.
   */
  def separateTheYoungFromTheOld(persons: List[Person]): List[List[String]] = {
    val perSort = persons.sortBy(_.age)
    val x = perSort.splitAt(perSort.indexOf(perSort.filter(person => person.age >= 18)(0)))
    List(x._1.map(person => person.firstName), x._2.map(person => person.firstName))
  }

}