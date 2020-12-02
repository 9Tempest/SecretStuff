/*
   Copyright 2020 Ondrej Lhotak. All rights reserved.

   Permission is granted for private study use by students registered in
   CS 241E in the Fall 2020 term.

   The contents of this file may not be published, in whole or in part,
   in print or electronic form.

   The contents of this file may be included in work submitted for CS
   241E assignments in Fall 2020. The contents of this file may not be
   submitted, in whole or in part, for credit in any other course.
*/
package cs241e.assignments

import cs241e.scanparse.DFAs._

object A7 {
  /** A sample DFA with alphabet {0,1} that recognizes binary integers that have no useless (leading) zeroes. */
  val binaryNumbers = DFA(
    alphabet = "01".toSet,
    states = Set("start", "0", "not0"),
    start = "start",
    accepting = Set("0", "not0"),
    transition = {
      case ("start", '0') => "0"
      case ("start", '1') => "not0"
      case ("not0", _) => "not0"
    })

  /** A DFA with alphabet {0,1} that recognizes binary integers that have no useless (leading) zeroes
    * and are not divisible by 3.
    */
  lazy val notDiv3 = DFA(
    alphabet = "01".toSet,
    states = Set("start", "0", "0mod3", "2mod3", "1mod3"),
    start = "start",
    accepting = Set("2mod3", "1mod3"),
    transition = {
      case ("start", '0') => "0"
      case ("start", '1') => "1mod3"
      case ("1mod3", '1') => "0mod3"
      case ("1mod3", '0') => "2mod3"
      case ("2mod3", '0') => "1mod3"
      case ("2mod3", '1') => "2mod3"
      case ("0mod3", '1') => "1mod3"
      case ("0mod3", '0') => "0mod3"
    })

  /** A DFA with alphabet {0,1} that recognizes binary integers that have no useless (leading) zeroes
    * and are not divisible by 2 or by 3.
    */
  lazy val notDiv23 = DFA(
    alphabet = "01".toSet,
    states = Set("start", "0", "0mod3", "2mod3Div2", "1mod3", "1mod3Div2", "2mod3NotDiv2"),
    start = "start",
    accepting = Set("1mod3", "2mod3NotDiv2"),
    transition = {
      case ("start", '0') => "0"
      case ("start", '1') => "1mod3"
      case ("1mod3", '1') => "0mod3"
      case ("1mod3", '0') => "2mod3Div2"
      case ("2mod3", '0') => "1mod3Div2"
      case ("1mod3Div2", '1') => "0mod3"
      case ("1mod3Div2", '0') => "2mod3Div2"
      case ("2mod3Div2", '1') => "2mod3NotDiv2"
      case ("2mod3Div2", '0') => "1mod3Div2"
      case ("2mod3NotDiv2", '1') => "2mod3NotDiv2"
      case ("2mod3NotDiv2", '0') => "1mod3Div2"
      case ("0mod3", '1') => "1mod3"
      case ("0mod3", '0') => "0mod3"
    })

  /** A DFA that recognizes a decimal number between -128 and 127 inclusive, with no useless zeroes.
    * (Zeroes are required and only permitted if removing them changes the meaning of the number.)
    * The alphabet symbols are {0,1,2,3,4,5,6,7,8,9,-}.
    */
  lazy val decimalNumber = DFA(
    alphabet = "-0123456789".toSet,
    states = Set("start","neg","0","InRange", "OutRange", "negInRange", "neg1", "1", "negOthersDec1","othersDec1", "negDec2Can","negDec2CanBound", "negDec2Cannot", "negDec2Can", "Dec2Cannot", "Dec2Can","Dec2CanBound", "invalid" ),
    start = "start",
    accepting = Set("0","InRange", "negInRange", "neg1", "1", "negOthersDec1","othersDec1", "negDec2Can", "negDec2Cannot", "negDec2Can", "Dec2Cannot", "Dec2Can","Dec2CanBound", "negDec2CanBound"),
    transition = {
      case ("start", '0') => "0"
      case ("start", '-') => "neg"
      case ("start", '1') => "1"
      case ("start", _) => "othersDec1"
      case ("neg", '1') => "neg1"
      case ("neg", '-') => "invalid"
      case ("neg", '0') => "invalid"
      case ("neg", _) => "negOthersDec1"
      case ("1", '0') => "Dec2Can"
      case ("1", '1') => "Dec2Can"
      case ("1", '2') => "Dec2CanBound"
      case ("1", '-') => "invalid"
      case ("1", _) => "Dec2Cannot"
      case ("neg1",'0') => "negDec2Can"
      case ("neg1",'1' ) => "negDec2Can"
      case ("neg1",'2') => "negDec2CanBound"
      case ("neg1", '-') => "invalid"
      case ("neg1", _) => "negDec2Cannot"
      case ("othersDec1", '-') => "invalid"
      case ("othersDec1", _) => "Dec2Cannot"
      case ("negOthersDec1", '-') => "invalid"
      case ("negOthersDec1", _) => "negDec2Cannot"
      case ("Dec2CanBound", '8') => "OutRange"
      case ("Dec2Can", '-') => "OutRange"
      case ("Dec2Can", _) => "InRange"
      case ("Dec2CanBound", '9') => "OutRange"
      case ("Dec2CanBound", '-') => "invalid"
      case ("Dec2CanBound", _) => "InRange"
      case ("Dec2Can", _) => "InRange"
      case ("Dec2Cannot", _) => "OutRange"
      case ("negDec2Can", '-') => "OutRange"
      case ("negDec2Can", _) => "InRange"
      case ("negDec2CanBound", '9') => "OutRange"
      case ("negDec2CanBound", '-') => "invalid"
      case ("negDec2CanBound", _) => "negInRange"
      case ("InRange", _) => "OutRange"
      case ("OutRange", _) => "OutRange"
      case ("negInRange", _) => "OutRange"
      case ("invalid", _) => "invalid"
    }
  )

  /** A DFA with alphabet {a, b, c} that recognizes any string that contains all three letters in
    * alphabetical order (i.e. "abc"), possibly interspersed with more letters. For example, "acbac"
    * and "cbacbacba" are in the language, but not "acba".
    */
  lazy val abc = DFA(
    alphabet = "abc".toSet,
    states = Set("start","hasNone", "hasA", "hasAB", "hasABC"),
    start = "start",
    accepting = Set("hasABC"),
    transition = {
      case ("start", 'a') => "hasA"
      case ("start", _) => "hasNone"
      case ("hasNone", 'a') => "hasA"
      case ("hasNone", _) => "hasNone"
      case ("hasA", 'b') => "hasAB"
      case ("hasA", _) => "hasA"
      case ("hasAB", 'c') => "hasABC"
      case ("hasAB", _) => "hasAB"
      case ("hasABC", _) => "hasABC"
    }
  )

  /** A DFA that recognizes any string from the alphabet {a,b,c} containing abc as a substring. */
  lazy val abcSubstring = DFA(
    alphabet = "abc".toSet,
    states = Set("start","hasNone", "hasA", "hasAB", "hasABC"),
    start = "start",
    accepting = Set("hasABC"),
    transition = {
      case ("start", 'a') => "hasA"
      case ("start", _) => "hasNone"
      case ("hasNone", 'a') => "hasA"
      case ("hasNone", _) => "hasNone"
      case ("hasA", 'b') => "hasAB"
      case ("hasA", 'a') => "hasA"
      case ("hasA", _) => "hasNone"
      case ("hasAB", 'a') => "hasA"
      case ("hasAB", 'c') => "hasABC"
      case ("hasAB", _) => "hasNone"
      case ("hasABC", _) => "hasABC"
    }
  )
}
