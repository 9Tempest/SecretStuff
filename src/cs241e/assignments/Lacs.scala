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

import Scanning._
import Parsing._
import Typer._
import CodeGenerator._
import Transformations._
import MemoryManagement._
import cs241e.nosource.ParsingPrivate
import cs241e.scanparse._
import Grammars._
import DFAs._
import cs241e.assignments.ProgramRepresentation.{Procedure, call}

/** Implementations of the definitions from the Lacs language specification. */

object Lacs {

  /** The set of keywords defined in the Lacs language specification. */
  val keywords = Set("def", "var", "Int", "if", "else")

  /** The set of single-character tokens in the Lacs language specification mapped to textual names.
    *
    * You may change the textual names if you wish, but you shouldn't need to.
    */
  val symbols = Map(
    ' ' -> "WHITESPACE",
    '\t' -> "WHITESPACE",
    '\n' -> "WHITESPACE",
    '\r' -> "WHITESPACE",
    '0' -> "ZERO",
    '<' -> "LT",
    '>' -> "GT",
    '=' -> "BECOMES",
    '+' -> "PLUS",
    '-' -> "MINUS",
    '*' -> "STAR",
    '/' -> "SLASH",
    '%' -> "PCT",
    '(' -> "LPAREN",
    ')' -> "RPAREN",
    '{' -> "LBRACE",
    '}' -> "RBRACE",
    ',' -> "COMMA",
    ';' -> "SEMI",
    ':' -> "COLON"
  )

    /** A DFA that recognizes any valid Lacs token from the list given in the Lacs specification,
      * (including COMMENT and WHITESPACE tokens).
      * The alphabet consists of every character that may appear in any token.
      */
  val keywordsState = Set("ID", "NUM", "DEF", "VAR", "INT", "IF", "ELSE", "EQ", "NE", "GE", "LE", "ARROW", "COMMENT")
  val dfa = {
    
    DFA(
      alphabet = "<>=+-*/%(){},;:! \t\n\r".toSet ++ ('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9'),
      states = Set("start", "COMMENT", "!") ++ symbols.values.toSet ++ keywordsState ,
      start = "start",
      accepting = keywordsState ++ symbols.values.toSet,
      transition = {
        case ("start", '0') => "ZERO"
        case start_num if (start_num._1 == "start" && start_num._2 <= '9' && start_num._2 >='1') => "NUM"
        case after_num if (after_num._1 == "NUM" && after_num._2 <= '9' && after_num._2 >='0') => "NUM"
        case ("start", ' ') => "WHITESPACE"
        case ("start", '\t') => "WHITESPACE"
        case ("start", '\n') => "WHITESPACE"
        case ("start", '\r') => "WHITESPACE"
        case ("start", '<') => "LT"
        case ("start", '>') => "GT"
        case ("start", '=') => "BECOMES"
        case ("start", '+') => "PLUS"
        case ("start", '-') => "MINUS"
        case ("BECOMES", '>') => "ARROW"
        case ("start", '*') => "STAR"
        case ("start", '/') => "SLASH"
        case ("SLASH", '/') => "COMMENT"
        case comment if (comment._1 == "COMMENT" && comment._2 != '\n') => "COMMENT"
        case ("start", '%') => "PCT"
        case ("start", '(') => "LPAREN"
        case ("start", ')') => "RPAREN"
        case ("start", '{') => "LBRACE"
        case ("start", '}') => "RBRACE"
        case ("start", ',') => "COMMA"
        case ("start", ';') => "SEMI"
        case ("start", ':') => "COLON"
        case ("start", '!') => "!"
        case ("BECOMES", '=') => "EQ"
        case ("LT", '=') => "LE"
        case ("GT", '=') => "GE"
        case ("!", '=') => "NE"
        //ID part
        case id_start if (id_start._1 == "start" && ((id_start._2 <= 'z' && id_start._2 >= 'a') || (id_start._2 <= 'Z' && id_start._2 >= 'A')) )=> "ID"
        case id_after if (id_after._1 == "ID" && ((id_after._2 <= 'z' && id_after._2 >= 'a') || (id_after._2 <= 'Z' && id_after._2 >= 'A') || (id_after._2 <= '9' && id_after._2 >= '0'))) => "ID"
      }
    )
  }

  /** A scanner for the Lacs programming language. Given an input string, scans it into a sequence of tokens.
    * The kinds of the tokens must correspond to the kinds defined in the Lacs specification
    * (e.g. ID, NUM, LPAREN, ...). WHITESPACE and COMMENT tokens are removed from the sequence. The resulting
    * sequence is returned with a `Token("BOF")` before it and a `Token("EOF")` after it.
    * If the input string cannot be scanned by the maximal munch algorithm, `sys.error()` is called
    * with a suitable error message.
    *
    * Do not forget to enforce the following rule in the Lacs specification:
    *
    * Pairs of consecutive tokens that both come from one of the following sets must be separated by at least
    * one WHITESPACE or COMMENT token:
    * {ID, DEF, VAR, INT, IF, ELSE, NUM}
    * {EQ, NE, LT, LE, GT, GE, BECOMES, ARROW}
   */

  def scan(input: String): Seq[Token] = {
    val set1 = Set("ID", "NUM", "DEF", "VAR", "INT", "IF", "ELSE")
    val set2 = Set("EQ", "NE", "GE", "LE", "ARROW", "LT", "GT", "BECOMES")
    var ans = Scanning.maximalMunchScan(dfa, input).toSeq
    ans = ans.map(x=>{
      if (x.kind == "ID") {
        if (x.lexeme == "def")  Token("DEF", "def")
        else if (x.lexeme == "if") Token("IF", "if")
        else if (x.lexeme == "else") Token("ELSE", "else")
        else if (x.lexeme == "Int") Token("INT", "Int")
        else if (x.lexeme == "var") Token("VAR", "var")
        else x
      }
      else if (x.kind == "ZERO") Token("NUM", "0")
      else x
    })
    var lastIndex = 0
    for (i <- (0 to ans.length-1 )) {
      if (i != 0){
        if (set1.contains(ans(i).kind) && set1.contains(ans(lastIndex).kind)) sys.error("Adjacent keywords from id set")
        if (set2.contains(ans(i).kind) && set2.contains(ans(lastIndex).kind)) sys.error("Adjacent keywords from cmp set")
        lastIndex += 1
      }
    }
    var finalAns = Seq[Token]()
    for (i <- ans){
      if (i.kind != "WHITESPACE" && i.kind != "COMMENT") finalAns = finalAns :+ i
    }
    finalAns = Token("BOF") +: finalAns :+ Token("EOF")
    finalAns
  }

  /** The grammar for the Lacs programming language copied from the language specification. */
  val grammar = parseGrammar("""
S BOF defdefs EOF
defdefs defdef defdefs
defdefs defdef
defdef DEF ID LPAREN parmsopt RPAREN COLON type BECOMES LBRACE vardefsopt defdefsopt expras RBRACE
parmsopt parms
parmsopt
parms vardef COMMA parms
parms vardef
vardef ID COLON type
type INT
type LPAREN typesopt RPAREN ARROW type
typesopt types
typesopt
types type COMMA types
types type
vardefsopt VAR vardef SEMI vardefsopt
vardefsopt
defdefsopt defdefs
defdefsopt
expras expra SEMI expras
expras expra
expra ID BECOMES expr
expra expr
expr IF LPAREN test RPAREN LBRACE expras RBRACE ELSE LBRACE expras RBRACE
expr term
expr expr PLUS term
expr expr MINUS term
term factor
term term STAR factor
term term SLASH factor
term term PCT factor
factor ID
factor NUM
factor LPAREN expr RPAREN
factor factor LPAREN argsopt RPAREN
test expr NE expr
test expr LT expr
test expr LE expr
test expr GE expr
test expr GT expr
test expr EQ expr
argsopt args
argsopt
args expr COMMA args
args expr
                             """
  )

  /** Scans and parses a Lacs program, returning the parse tree. */
  def scanAndParse(input: String): Tree = {
    val tokens = scan(input).toIndexedSeq
    val tree = parseCYK(grammar, tokens).getOrElse{
      val longestPrefixKinds = ParsingPrivate.longestPrefix(grammar, tokens)
      val longestPrefixLexemes = tokens.map(_.lexeme).take(longestPrefixKinds.length).mkString(" ")
      sys.error("Parsing error; longest prefix: "+longestPrefixLexemes)
    }
    tree
  }

  /** Scans, parses, and type-checks a Lacs program. Returns the `ProcedureScope`s representing the procedures
    * and a map giving the `Type` of each `Tree` that has one.
    */
  def scanAndParseAndType(input: String): TypedProcedures = {
    val tree = scanAndParse(input)
    typeTree(tree)
  }

  /** Scans, parses, and type-checks a Lacs program, and generates procedures from it. Returns the corresponding
    * `Procedure` objects.
    */
  def scanAndParseAndTypeAndGenerate(input: String): Seq[Procedure] = {
    val typedProcedures = scanAndParseAndType(input)
    generateProcedures(typedProcedures)
  }

  /** Compiles a Lacs program to MIPS machine language. */
  def compile(input: String): MachineCode = {
    val procedures = scanAndParseAndTypeAndGenerate(input)
    compilerA6(procedures)
  }

  /** Compiles a Lacs program and the `GarbageCollector` to MIPS machine language. */
  def compileWithGarbageCollector(input: String): MachineCode = {
    MemoryManagement.heap = GarbageCollector
    val procedures = scanAndParseAndTypeAndGenerate(input)
    compilerA6(procedures ++ GarbageCollector.procedures)
  }
}
