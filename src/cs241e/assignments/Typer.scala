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

import ProgramRepresentation._
import cs241e.scanparse.Grammars._

import scala.collection.mutable

/** Implementation of semantic analysis for the Lacs language. */

object Typer {
  /** Representation of a Lacs type, which is either an Int or a function type with parameter types and a return type.
    */
  sealed abstract class Type
  case object IntType extends Type
  case class FunctionType(parameterTypes: Seq[Type], returnType: Type) extends Type

  /** Given a `tree`, finds all descendants of the `tree` whose root node has kind `lhsKind`.
    * Does not search within the found subtrees for any nested occurrences of additional descendants.
    *
    * For example, searching the root of a program tree with `lhsKind = "procedure"` will return the trees all
    * of the top-level procedures, but not any procedures nested within them.
    */
  def collect(tree: Tree, lhsKind: String): Seq[Tree] =
    if(tree.lhs.kind == lhsKind) Seq(tree) else tree.children.flatMap((tree: Tree) => collect(tree, lhsKind))

  /** Given a tree that is either a "type" or contains exactly one "type" nested within it, returns
    * an instance of `Type` representing the corresponding type.
    */
  def parseType(tree: Tree): Type = {
    val types = collect(tree, "type")
    require(types.size == 1)
    //println(types.head.lhs.kind)
    if (types.head.children.length == 1) return IntType
    var func = Seq[Tree]()
    for (i <- types.head.children){
      func = func ++ collect(i, "type")
    }
    var ans = Seq[Tree]()
    for (i <- 0 to func.length-2){
      ans = ans.appended(func(i))
    }
    FunctionType(ans.map(parseType), parseType(func(func.length-1)))
  }

  /** A variable combined with its declared type. */
  case class TypedVariable(variable: Variable, tpe: Type)

  /** Create a new `Variable` given its `name` and type `tpe`. */
  def makeVariable(name: String, tpe: Type): Variable =
    new Variable(name, isPointer = (tpe != IntType))

  /** A `SymbolTable` maps each name to either a `TypedVariable` or a `ProcedureScope`. */
  type SymbolTable = Map[String, Either[TypedVariable, ProcedureScope]]

  /** Given a tree containing subtrees rooted at "vardef", creates a `TypedVariable` for each such tree. */
  def parseVarDefs(tree: Tree): Seq[TypedVariable] = {
    collect(tree, "vardef").map{ varDef => {
      //println(varDef.production)
      //println(collect(varDef, "type").length)
      val t = parseType(varDef)
      TypedVariable(makeVariable(collect(varDef, "ID").head.lhs.lexeme, t), t)
    } }
  }

  /** Call `sys.error()` if any `String` occurs in `names` multiple times. */
  def checkDuplicates(names: Seq[String]): Unit = {
    val duplicates = names.diff(names.distinct)
    if(duplicates.nonEmpty) sys.error(s"Duplicate identifiers ${duplicates}")
  }

  /** A `ProcedureScope` holds the semantic information about a particular procedure that is needed to type-check
    * the body of the procedure, including information coming from outer procedure(s) within which this
    * procedure may be nested.
    *
    * @param tree the tree defining the procedure (rooted at a "defdef")
    * @param outer the `ProcedureScope` of the outer procedure that immediately contains this one
    */
  class ProcedureScope(tree: Tree, outer: Option[ProcedureScope] = None) {
    assert(tree.production ==
      "defdef DEF ID LPAREN parmsopt RPAREN COLON type BECOMES LBRACE vardefsopt defdefsopt expras RBRACE")
    val Seq(_, id, _, parmsopt, _, _, retTypeTree, _, _, vardefs, defdefs, expras, _) = tree.children

    /** The name of the procedure. */
    val name: String = id.lhs.lexeme

    /** The parameters of the procedure. */
    val parms: Seq[TypedVariable] = parseVarDefs(parmsopt)

    /** The variables declared in the procedure. */
    val variables: Seq[TypedVariable] = parseVarDefs(vardefs)

    /** The declared return type of the procedure. */
    val returnType: Type = parseType(retTypeTree)

    /** The new `Procedure` object that will represent this procedure. */
    val procedure: Procedure = {

      if (outer != None) {

        val ans = new Procedure(name, parms.map(x=>x.variable), Some(outer.get.procedure))
         ans
      }
      else new Procedure(name, parms.map(x=>x.variable))
    }

    /** The `ProcedureScope`s of the nested procedures that are immediately nested within this procedure.
      *
      * Note: this `val` will recursively call `new ProcedureScope(...)`.
      */
    val subProcedures: Seq[ProcedureScope] = {

      def transform(tree: Tree):Seq[ProcedureScope] = {

        if (tree.children.length == 1) return Seq(new ProcedureScope(tree.children(0), Some(this)))
        transform(tree.children(1)) :+ new ProcedureScope(tree.children(0), Some(this))
      }
      if (defdefs.children.length == 0)  Seq()
      else transform(defdefs.children.head).reverse
    }

    /** The names of parameters, variables, and nested procedures that are newly defined within this procedure
      * (as opposed to being inherited from some outer procedure).
      */
    val newNames: Seq[String] = {
      val param = parms.map(x=>x.variable.name)
      val vars = variables.map(x=>x.variable.name)
      val procs = subProcedures.map(x=>x.name)
      param ++ vars ++ procs
    }
    checkDuplicates(newNames)

    /** Create and return a symbol table to be used when type-checking the body of this procedure. It
      * should contain all symbols (parameters, variables, nested procedures) defined in this procedure,
      * as well as those defined in outer procedures within which this one is nested. Symbols defined in
      * this procedure override (shadow) those of outer procedures. The `outerSymbolTable` parameter
      * contains the symbol table of the enclosing scope (either an outer procedure within which the
      * current procedure is nested, or, if the current procedure is a top-level procedure, a symbol
      * table containing the names of all of the top-level procedures).
      */
    def symbolTable(outerSymbolTable: SymbolTable): SymbolTable = {
      val ans1 = (subProcedures.map(x=>(x.name->Right(x))) ++ (parms++variables).map(x=>(x.variable.name->Left(x)))).toMap
      outerSymbolTable ++ ans1
    }

    /** Returns a sequence containing `this` `ProcedureScope` and the `ProcedureScope`s for all procedures
      * declared inside of this procedure, including those nested recursively within other nested procedures.
      *
      * Scala hint: learn about the `flatMap` method in the Scala library. If you are not familiar with flatMap,
      * one place you can read about it is here:
      * http://www.artima.com/pins1ed/working-with-lists.html#sec:higher-order-methods
      */
    def descendantScopes: Seq[ProcedureScope] = {
      val ans = Seq[ProcedureScope](this)
      ans ++ subProcedures.flatMap(x => x.descendantScopes)
    }
  }

  /** Creates a map containing a symbol table for each procedure scope by calling the scope's symbolTable method,
    * passing in the symbol table of its outer enclosing procedure (or the top level symbol table for a top level
    * procedure).
    */
  def createSymbolTables(topLevelProcedureScopes: Seq[ProcedureScope], topLevelSymbolTable: SymbolTable):
    Map[ProcedureScope, SymbolTable] = {
    def recur(procedureScopes: Seq[ProcedureScope], outerSymbolTable: SymbolTable): Map[ProcedureScope, SymbolTable] = {
      procedureScopes.flatMap{ procedureScope =>
        val symbolTable = procedureScope.symbolTable(outerSymbolTable)
        Map(procedureScope -> symbolTable) ++ recur(procedureScope.subProcedures, symbolTable)
      }.toMap
    }
    recur(topLevelProcedureScopes, topLevelSymbolTable)
  }

  /** Checks that the body of a procedure satisfies the type-checking rules in the Lacs language specification.
    * Returns a `Map` that provides a `Type` for each `Tree` that has a `Type` according to the language
    * specification.
    */

  def typeCheck(scope: ProcedureScope, symbolTable: SymbolTable): Map[Tree, Type] = {
    /** The map that will be returned containing the `Type` of each `Tree` that has a `Type`. */
    val treeToType = mutable.Map[Tree, Type]()

    /** Calls `sys.error()` if `tpe1` and `tpe2` are not equal. If they are equal, returns them. */
    def mustEqual(tpe1: Type, tpe2: Type): Type =
      if(tpe1 == tpe2) tpe1 else sys.error(s"Type mismatch: expected $tpe2, got $tpe1")

    /** For a `tree` rooted at a node that has a `Type`, computes the `Type`, adds it to `treeToType`,
      * and returns it.
      */

    def getArgs(tree: Tree):Seq[Type] = {
      if (tree.children.length == 1) return Seq(typeOf(tree.children.head))
      getArgs(tree.children(2)):+typeOf(tree.children(0))
    }
    def typeOf(tree: Tree): Type = treeToType.getOrElseUpdate(tree, {
      if (tree.lhs.kind == "NUM")  IntType
      else if (tree.lhs.kind == "ID") {
        if (!symbolTable.contains(tree.lhs.lexeme)) sys.error(s"${tree.lhs.lexeme} has not been defined")
        val ans = symbolTable(tree.lhs.lexeme)
        ans match {
          case Right(x)=>FunctionType(x.parms.map(p=>p.tpe),x.returnType)
          case Left(x)=> x.tpe
        }
      }
      else if (tree.children.length == 3 && (tree.children(1).lhs.kind == "PLUS" || tree.children(1).lhs.kind == "MINUS"
        || tree.children(1).lhs.kind == "STAR" || tree.children(1).lhs.kind == "SLASH" || tree.children(1).lhs.kind == "PCT")) {
        mustEqual(typeOf(tree.children(0)), IntType)
        mustEqual(typeOf(tree.children(2)), IntType)
      }
      else if (tree.children.length == 3 && (tree.children(1).lhs.kind == "BECOMES")){
        mustEqual(typeOf(tree.children(0)), typeOf(tree.children(2)))
        val ans = symbolTable(tree.children(0).lhs.lexeme) match {
          case Left(x)=>x.tpe
          case Right(x)=>sys.error("Cannot assign value to a procedure")
        }
        typeOf(tree.children(2))
      }
      else if (tree.children.length == 3 && tree.children(1).lhs.kind == "SEMI"){
        typeOf(tree.children(0))
        typeOf(tree.children(2))
      }
      else if (tree.children.length == 3 && tree.children(0).lhs.kind == "LPAREN") typeOf(tree.children(1))
      else if (tree.lhs.kind == "test"){
        mustEqual(typeOf(tree.children(0)),IntType)
        mustEqual(typeOf(tree.children(2)),IntType)
      }
      else if (tree.children.length == 11){
        typeOf(tree.children(2))
        mustEqual(typeOf(tree.children(5)), typeOf(tree.children(9)))
      }
      else if (tree.children.length == 4){
        val ans = typeOf(tree.children(0)) match {
          case IntType => sys.error("invalid type")
          case FunctionType(params, ret) => {
            if (tree.children(2).children.isEmpty) {
              if (params.length != 0) sys.error("param length does not match argument length")
              return ret
            }
            val args = getArgs(tree.children(2).children(0)).reverse
            if (params.length != args.length) sys.error("param length does not match argument length")
            for (i <- 0 to args.length-1){
              mustEqual(params(i), args(i))
            }
            ret
          }
        }
        ans
      }
      else typeOf(tree.children.head)
    })

    /* Check that the type of the expression returned from the procedure matches the declared type of the procedure. */
    mustEqual(scope.returnType, typeOf(scope.expras))
    
    Map() ++ treeToType
  }

  case class TypedProcedures(
                              procedureScopes: Seq[ProcedureScope],
                              symbolTables: ProcedureScope=>SymbolTable,
                              typeMap: Tree=>Type)

  /** Type-checks a Lacs program parse tree. Returns `TypedProcedures`, which contains the `ProcedureScope`s
    * representing the procedures, a map giving a `SymbolTable` for each `ProcedureScope`,
    * and a map giving the `Type` of each `Tree` that has one.
    */
  def typeTree(tree: Tree): TypedProcedures = {
    assert(tree.production == "S BOF defdefs EOF")
    val defdefs = tree.children(1)

    val topLevelProcedureScopes = collect(defdefs, "defdef").map{defdef => new ProcedureScope(defdef, None)}
    checkDuplicates(topLevelProcedureScopes.map(procedureScope => procedureScope.name))
    val topLevelSymbolTable: SymbolTable =
      topLevelProcedureScopes.map{procedure => (procedure.name -> Right(procedure))}.toMap
    val symbolTables = createSymbolTables(topLevelProcedureScopes, topLevelSymbolTable)

    val allProcedureScopes = topLevelProcedureScopes.flatMap(procedureScope => procedureScope.descendantScopes)

    val typeMap: Map[Tree, Type] = allProcedureScopes.flatMap(procedureScope =>
      typeCheck(procedureScope, symbolTables(procedureScope))).toMap

    val mainProcedure = topLevelProcedureScopes.head
    if(mainProcedure.returnType != IntType
      || mainProcedure.parms.map(_.tpe) != Seq(IntType, IntType))
      sys.error("The type of the main procedure must be (Int, Int)=>Int.")

    TypedProcedures(allProcedureScopes, symbolTables, typeMap)
  }
}
