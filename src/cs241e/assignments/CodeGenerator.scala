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

import cs241e._
import ProgramRepresentation._
import CodeBuilders._
import tools._
import Typer._
import scanparse.Grammars._

/** A code generator that converts a Lacs parse tree into the intermediate language developed in Assignment 1 to 6. */
object CodeGenerator {
  def generateProcedures(typedProcedures: TypedProcedures) = {

    /** Given a `procedureScope`, generates the `Code` implementing the procedure, and assigns it to
      * `procedureScope.procedure.code`.
      */
    def generateCode(procedureScope: ProcedureScope): Unit = {

      /** Generates the `Code` that implements `tree`.
        *
        * This method will be called from the outside only on `tree`s rooted at nodes of kind "expras".
        * However, if it calls itself recursively on other kinds of nodes, it needs to be able to handle
        * those node kinds as well.
        */


      def recur(tree: Tree): Code = {

        def getVar(tree:Tree): Either[TypedVariable, ProcedureScope]={
          if (tree.lhs.kind != "ID" && tree.children.length == 3) {
            return getVar(tree.children(1))
          } else if (tree.lhs.kind != "ID"){
            return getVar(tree.children(0))
          }

          typedProcedures.symbolTables(procedureScope)(tree.lhs.lexeme)

        }
        def getArgs(tree:Tree): Seq[Code] = {
          def getArgsHelper(tree:Tree) :Seq[Code]={
            if (tree.children.length == 1) Seq(recur(tree.children.head))
            else getArgsHelper(tree.children(2)) :+ recur(tree.children.head)
          }
          if (tree.children.isEmpty) return Seq()
          getArgsHelper(tree.children(0)).reverse
        }

        def getParams(tree:Tree):Seq[Variable] = {
          def helper(tree:Tree):Seq[Variable] = {
            if (tree.children.length == 1) Seq(typedProcedures.typeMap(tree.children(0)) match {
              case IntType => new Variable("temp")
              case _ => new Variable("temp", isPointer = true)
            })  else {
              helper(tree.children(2)) :+ (typedProcedures.typeMap(tree.children(0)) match {
                case IntType => new Variable("temp")
                case _ => new Variable("temp", isPointer = true)
              })
            }
          }
          if (tree.children.length == 0) Seq()
          else helper(tree.children(0)).reverse
        }

        if (tree.lhs.kind == "NUM") const(tree.lhs.lexeme.toInt)
          /*
        else if (tree.lhs.kind == "ID") typedProcedures.symbolTables(procedureScope)(tree.lhs.lexeme) match {
          case Left(value) => read(Reg.result, value.variable)
          case Right(value) => Closure(value.procedure)
        }*/
        else if (tree.children.length == 1 && tree.children(0).lhs.kind == "ID") typedProcedures.symbolTables(procedureScope)(tree.children(0).lhs.lexeme) match {
          case Left(value) => read(Reg.result, value.variable)
          case Right(value) => Closure(value.procedure)
        }
        else if (tree.lhs.kind == "expra" && tree.children.length == 3) {
          typedProcedures.symbolTables(procedureScope)(tree.children(0).lhs.lexeme) match {
            case Left(x)=>assign(x.variable, recur(tree.children(2)))
          }
        } else if (tree.children.length == 3 && tree.lhs.kind != "test"){
          if (tree.children(1).lhs.kind == "PLUS"){
            binOp(recur(tree.children(0)), plus, recur(tree.children(2)))
          } else if (tree.children(1).lhs.kind == "MINUS"){
            binOp(recur(tree.children(0)), minus, recur(tree.children(2)))
          } else if (tree.children(1).lhs.kind == "STAR"){
            binOp(recur(tree.children(0)), times, recur(tree.children(2)))
          } else if (tree.children(1).lhs.kind == "SLASH"){
            binOp(recur(tree.children(0)), divide, recur(tree.children(2)))
          } else if (tree.children(1).lhs.kind == "PCT"){
            binOp(recur(tree.children(0)), remainder, recur(tree.children(2)))
          } else if (tree.children(1).lhs.kind == "SEMI"){
            block(
              recur(tree.children(0)),
              recur(tree.children(2))
            )
          } else recur(tree.children(1))
        } else if (tree.children.length == 11){
          if (tree.children(2).children(1).lhs.kind == "NE") ifStmt(recur(tree.children(2).children(0)), neCmp, recur(tree.children(2).children(2)), recur(tree.children(5)), recur(tree.children(9)))
          else if (tree.children(2).children(1).lhs.kind == "EQ") ifStmt(recur(tree.children(2).children(0)), eqCmp, recur(tree.children(2).children(2)), recur(tree.children(5)), recur(tree.children(9)))
          else if (tree.children(2).children(1).lhs.kind == "LT") ifStmt(recur(tree.children(2).children(0)), ltCmp, recur(tree.children(2).children(2)), recur(tree.children(5)), recur(tree.children(9)))
          else if (tree.children(2).children(1).lhs.kind == "LE") ifStmt(recur(tree.children(2).children(0)), leCmp, recur(tree.children(2).children(2)), recur(tree.children(5)), recur(tree.children(9)))
          else if (tree.children(2).children(1).lhs.kind == "GT") ifStmt(recur(tree.children(2).children(0)), gtCmp, recur(tree.children(2).children(2)), recur(tree.children(5)), recur(tree.children(9)))
          else  ifStmt(recur(tree.children(2).children(0)), geCmp, recur(tree.children(2).children(2)), recur(tree.children(5)), recur(tree.children(9)))
        } else if (tree.children.length == 4){
          getVar(tree.children(0)) match {
            case Left(x) => {
              val args = getArgs(tree.children(2))
              CallClosure(recur(tree.children(0)), args, getParams(tree.children(2)))
            }
            case Right(x) => {
              val args = getArgs(tree.children(2))
              if (x.returnType == IntType){
                Call(x.procedure, args)
              } else {

                CallClosure(recur(tree.children(0)), args, getParams(tree.children(2)))

              }
            }
          }
        } else {
          recur(tree.children(0))
        }
      }

      /* Main body of generateCode. */
      procedureScope.procedure.code = Scope(procedureScope.variables.map(_.variable), recur(procedureScope.expras))
    }

    /* Main body of generateProcedures. */

    typedProcedures.procedureScopes.foreach(generateCode)
    typedProcedures.procedureScopes.map(x=>{
      x.procedure
    })
  }
}
