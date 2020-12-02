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

import scala.annotation.tailrec
import cs241e.scanparse.DFAs._


/** Implementations of a DFA recognizer and a scanner. */
object Scanning {



  /** Given a DFA and an input string, determines whether the input string is in the language specified by the DFA. */
  def recognize(dfa: DFA, input: List[Char]): Boolean = {
    var curr = dfa.start
    for (i <- input){
      if (!dfa.alphabet.contains(i)) return false
      if (!dfa.transition.isDefinedAt(curr,i)) return false
      curr = dfa.transition(curr, i)
      if (!dfa.states.contains(curr)) return false
    }
    if (!dfa.accepting.contains(curr)) return false
    true
  }



  /** Given a DFA and an input string, splits the input string into tokens using the maximal munch algorithm.
    * Specifically, each token is the longest prefix of the remaining input string that is in the language
    * specified by the DFA. If the input string cannot be tokenized in this way, calls sys.error() with an
    * appropriate error message.
    */
  def maximalMunchScan(dfa: DFA, input: String): Seq[Token] = {

    /** Scans a single token. More precisely, finds the longest prefix of `input` accepted by `dfa` if the `dfa`
      * starts executing in `state`.
      *
      * You may implement this method with a loop instead of recursion if you prefer, but it is more complicated
      * and more tricky to get it right.
      *
      * @param input the input string to be scanned
      * @param state the state in which the DFA is to start executing
      * @param backtrack the value to return if the DFA gets stuck (if the transition function is not defined
      *                  for the current combination of state and next symbol from the input)
      * @return a pair of the rest of the `input` after the accepted prefix has been removed and the final state
      *         of the `dfa` after it has executed from `state` on the accepted prefix
      */
    
    def scanOne(input: List[Char], state: State, backtrack: (List[Char], State) ): (List[Char], State) = {
      if (input.isEmpty && backtrack._2 == dfa.start) {
        sys.error("Input is empty")
      } else if (input.isEmpty){
        backtrack
      } else if (!dfa.transition.isDefinedAt((state, input(0))) && backtrack._2 == dfa.start){
        sys.error("Cannot tokenized")
      } else if (!dfa.transition.isDefinedAt((state, input(0)))) {
        backtrack
      } else if (dfa.accepting.contains(dfa.transition((state, input(0))))){
        scanOne(input.tail, dfa.transition(state, input(0)), (input.tail, dfa.transition(state, input(0))))
      }else {
        scanOne(input.tail, dfa.transition(state, input(0)), backtrack)
      }
    }

    /** Given a `list` and `rest` such that `list.tail.tail....tail eq rest` for some sequence of zero or more
      * .tail operations, returns a list containing the elements that are in `list` but not in `rest` in the
      * same order that they appear in `list`.
      *
      * Scala hint: `==` returns true if two lists contain equal elements, like equal? in Racket.
      * `eq` returns true if two lists are physically the same list, like eq? in Racket.
      */
    def listDiff[A](list: List[A], rest: List[A]): List[A] =
      if (list eq rest) Nil
      else list.head :: listDiff(list.tail, rest)

    /** The core of the maximal munch scanning algorithm. Repeatedly calls `scanOne` on the remaining
      * `input` until the input is empty, in which case it returns the list of tokens that have been
      * scanned, in reverse order. The `accumulator` accumulates the tokens in the style of generative
      * recursion. If `scanOne` makes no progress (scans the empty prefix), calls `sys.error()` with
      * a suitable error message. Each token should contain the string of characters scanned (the lexeme)
      * and the state in which the DFA ended up after scanning the lexeme (which will be used as the kind
      * of the token).
      *
      * Hint: the `listDiff` method will be useful.
      *
      * This method can also be implemented using a loop, and you are free to do so if you prefer.
      */
    
    def recur(input: List[Char], accumulator: List[Token] = List.empty): List[Token] = {
      if (input.isEmpty) accumulator
      else {
        val ans = scanOne(input, dfa.start, (input, dfa.start))

        recur(ans._1, Token(ans._2, listDiff(input, ans._1).mkString) :: accumulator)

      }
    }

    recur(input.toList).reverse
  }
}
