package org.scalalabs.basic.lab03

/**
 * This exercise introduces you to the powerful pattern matching features of Scala.
 *
 * Pattern matching can in its essence be compared to Java's 'switch' statement, even though it provides
 * many more possibilites. Whereas the Java switch statmenet lets you 'match' primitive types up to int's,
 * Scala's pattern matching goes much further. Practically everything from all types of objects and Collections
 * can be matched, not forgetting xml and a special type of class called case classes.
 *
 * For this exercise exclusively use pattern matching constructs in order to make the corresponding unittest work.
 *
 * Reference material to solve these exercises can be found here:
 * Pattern matching in general: http://programming-scala.labs.oreilly.com/ch03.html#PatternMatching
 * Pattern matching in combination with partial functions: http://programming-scala.labs.oreilly.com/ch08.html#PartialFunctions
 */

object PatternMatchingExercise {

  /**
   * ***********************************************************************
   *  pattern matching exercises
   * For expected solution see unittest @PatternMatchingExerciseTest
   * ***********************************************************************
   */

  def describeLanguage(s: String) = {
    s match {
      case "Clojure" | "Haskell" | "Erlang" => "Functional"
      case "Scala" => "Hybrid"
      case "Java" | "Smalltalk" => "OOP"
      case "C" => "Procedural"
      case _ => "Unknown"
    }
  }

  def matchOnInputType(in: Any) = in match {
    case s: String => s"A string with length ${s.length}"
    case i: Int if i > 0 => "A positive integer"
    case p @ Person(_,_) => s"A person with name: ${p.name}"
    case s: Seq[_] if (s.size > 10) => "Seq with more than 10 elements"
    case first :: second :: tail => s"first: $first, second: $second, rest: $tail"
    case o: Option[_] => "A Scala Option subtype"
    case a: AnyRef => "Some Scala class"
    case null => "A null value"
    case _ => "The default"
  }

  def older(p: Person): Option[String] = p match {
    case Person(name, age) if age > 30 => Some(name)
    case _ => None
  }

  /**
   * ***********************************************************************
   * Pattern matching with partial functions
   * For expected solution see @PatternMatchingExerciseTest
   * ***********************************************************************
   */
  val pf1: PartialFunction[String, String] = {
    case "scala-labs" => "Got scala-labs"
    case "stuff" => "Got stuff"
  }

  val pf2: PartialFunction[String, String] = {
    case "other stuff" => "Got stuff"
  }

  val pf3 = {
    pf1 orElse pf2
  }
}

case class Person(name: String, age: Int)