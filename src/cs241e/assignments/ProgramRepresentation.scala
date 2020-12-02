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

import cs241e.mips
import mips._
import Debugger.DebugTable
import Assembler._

import scala.collection.mutable

/** Defines the elements that are composed to represent programs in the internal representation of the
  *  CS 241E compiler.
  */
object ProgramRepresentation {
  /* The following are used starting in Assignment 2. */

  /** A label representing a textual name for a given memory address. */
  class Label(val name: String) {
    override def toString = s"Label($name)"
  }

  /** The overall abstract class representing a piece of code. It can be instantiated as any of the
    * subclasses that extend `Code`. */
  sealed abstract class Code

  /** A machine language instruction represented as a 32-bit `Word`. */
  case class CodeWord(word: Word) extends Code

  /** Enables the use of a `Word` wherever a `Code` is expected by wrapping the `Word` in a `CodeWord`. */
  implicit def toCodeWord(word: Word): Code = CodeWord(word)

  /** A definition of a label. The given label is associated with the memory address at which the
    * code immediately following the `Define` will be loaded in memory.
    */
  case class Define(label: Label) extends Code

  /** A use of a label. Once the addresses associated with all labels are known, each `Use` will be replaced
    * by a 32-bit word, the address that is associated with the given label.
    */
  case class Use(label: Label) extends Code

  /** A BEQ or BNE instruction whose target address is specified by a label. Once the addresses associated
    * with all labels are known, each `BeqBne` will be replaced by a 32-bit word representing a BEQ or BNE
    * instruction that transfers control to the given label if the associated condition is satisfied.
    * The `bits` parameter are the first 16 bits of the BEQ or BNE instruction to be generated. The last
    * 16 bits will be filled in with a branch offset that is determined once both the memory address
    * of the BEQ or BNE instruction and the memory address associated with the label are known.
    *
    * See the `beq` and `bne` methods in `ProgramRepresentation.scala` for a convenient way to create a BeqBne.
    */
  case class BeqBne(bits: Seq[Boolean], label: Label) extends Code

  /** Creates a `BeqBne` that generates a BEQ instruction that branches to the address associated with `label`. */
  def beq(s: Reg, t: Reg, label: Label): BeqBne = BeqBne(BEQ(s, t, 0).take(16), label)

  /** Creates a `BeqBne` that generates a BNE instruction that branches to the address associated with `label`. */
  def bne(s: Reg, t: Reg, label: Label): BeqBne = BeqBne(BNE(s, t, 0).take(16), label)


  /** A comment which is removed when the machine language code is generated. The `message` is retained in
    * a separate table so that it can be shown by the `Debugger` when it executes the generated code.
    */
  case class Comment(message: String) extends Code

  /** Given a sequence `codes` of `Code`, generates a table suitable for use by the `Debugger`. The table
    * associates with each memory address the comments appearing just before the code at that address,
    * and the labels defined at that address. The `Debugger` uses this table to show labels and comments
    * when it traces through the execution of machine language code.
    *
    * Note: this method assumes that the `codes` contain only `CodeWord`, `Define`, `Use`, `BeqBne`,
    * or `Comment`.
    */
  def createDebugTable(codes: Seq[Code]): DebugTable = {
    val ret = mutable.Map[mips.Word,String]()
    var location = 0
    def locationWord = Word(encodeUnsigned(location * 4))
    def add(msg: String): Unit = {
      val existing = ret.getOrElse(locationWord, "")
      ret(locationWord) = existing + msg + "\n"
    }
    for(code <- codes) code match {
      case _:CodeWord | _:Use | _:BeqBne => location += 1
      case Comment(msg) => add(msg)
      case Define(label) => add(label.toString)
      case _ => require(false, s"Encountered unsupported code $code.")
    }
    Map() ++ ret
  }

  /** A block representing a sequence of pieces of code. A block may have other blocks nested within it. */
  case class Block(stmts: Seq[Code]) extends Code

  /** Creates a `Block` containing the given `codes`.
    *
    * This is for convenience so that we do not have to always write `Seq` inside the block.
    * The following are equivalent:
    * Block(Seq(code1, code2, code3)) == block(code1, code2, code3)
    */
  def block(codes: Code*): Block = Block(codes)

  /* The following are used starting in Assignment 3. */

  /** Represents an abstract named variable. The compiler eventually decides where in memory the value of
    * the variable will be stored, and generates the corresponding code to read and write the value.
    *
    * The `isPointer` parameter should be set to `true` whenever the value of the variable is intended
    * to be interpreted as the memory address of a `Chunk` (see MemoryManagement.scala).
    */
  class Variable(val name: String, val isPointer: Boolean = false) {
    override def toString = s"Variable($name)"
  }

  /** Code that reads a value from `variable` to register `register` (if `read` is true), or writes the
    * value of `register` to `variable` (if `read` is false). We do not allow `Reg.scratch` to be written
    * to enable us to later use `Reg.scratch` in the machine language code that implements a `VarAccess`.
    */
  case class VarAccess(register: Reg, variable: Variable, read: Boolean) extends Code {
    assert(read || register != Reg.scratch)
    override def toString =
      if(read) s"read $variable into $$$register"
      else s"write $$$register into $variable"
  }

  /** Generates a `VarAccess` that reads `variable` into `register`. */
  def read(register: Reg, variable: Variable) = VarAccess(register, variable, read = true)

  /** Generates a `VarAccess` that writes `register` into `variable`. */
  def write(variable: Variable, register: Reg) = VarAccess(register, variable, read = false)

  /** Generates an assignment statement that first evaluates `expr` (assuming its result is stored into `Reg.result`),
    * then writes the result into `variable`.
    */
  def assign(variable: Variable, expr: Code) = block(expr, write(variable, Reg.result))

  /* The following are used starting in Assignment 4. */

  /** A declaration of variables with a piece of code in which those variables can be accessed. */
  case class Scope(variables: Seq[Variable], code: Code) extends Code

  /** An if statement that evaluates the expressions `e1` and `e2`, compares them using `comp`, and transfers
    * control to either `thens` or `elses` depending on whether the comparison succeeded or failed, respectively.
    * `comp` should assume that the value of `e1` is in register `Reg.scratch` and the value of `e2` is in register
    * `Reg.result`. It should return code that branches to the `elseLabel` if the comparison fails, or
    * continue executing the following instruction if the comparison succeeds. The `ifStmt` method is
    * a convenience method that constructs `IfStmt` without requiring the `elseLabel` to be externally
    * created. It creates the `elseLabel` itself.
    */
  def ifStmt(e1: Code, comp: Label=>Code, e2: Code, thens: Code, elses: Code = Block(Seq())) = {
    val elseLabel = new Label("else")
    IfStmt(elseLabel, e1, comp(elseLabel), e2, thens, elses)
  }
  case class IfStmt(elseLabel: Label, e1: Code, comp: Code, e2: Code, thens: Code, elses: Code) extends Code

  /* The following are used starting in Assignment 5. */

  /** A call to `procedure`, passing the results of evaluating `arguments` as arguments.
    * 
    * It is always safe to leave `isTail` as `false`. `isTail` may be set to `true` if the `Call` is the last thing
    * that can happen in the current procedure. When `isTail` is true, the generated code will return from the
    * called procedure directly to the caller of the current procedure, skipping the current procedure on the
    * return.
    */
  case class Call(procedure: Procedure, arguments: Seq[Code], isTail: Boolean = false) extends Code {
    assert(procedure.parameters.size == arguments.size)
  }

  /** Helper method that generates a `Call` with an arbitrary number of arguments without
    * having to wrap the arguments in a `Seq`. In general, the following are equivalent:
    * call(proc, arg1, arg2) == Call(proc, Seq(arg1, arg2))
    */
  def call(procedure: Procedure, arguments: Code*): Call = Call(procedure, arguments)


  /** A representation of a procedure.
    * 
    * @param name a name for the procedure
    * @param parameters the parameters of the procedure, which may be used in the `code`
    * @param outer if the procedure is nested inside some outer procedure, then `outer` should refer to the outer
    *              procedure
    *              
    * The above parameters are required when the `Procedure` is constructed. The `code` can be filled in later,
    * after the procedure has been constructed. This is necessary to allow recursive procedures: the `code` of
    * a recursive procedure needs to contain a `Call` that refers to the `Procedure` itself, but this reference
    * could not be created before the `Procedure` itself is constructed.
    */
  class Procedure(val name: String,
                  val parameters: Seq[Variable],
                  val outer: Option[Procedure] = None) {
    /** The nesting depth of the procedure (the number of outer procedures inside which it is nested). */
    lazy val depth: Int = {
      if (outer == None) 0
      else outer.get.depth+1
    }

    /** Holds the address of the `Chunk` of parameters that was passed in from the caller. */
    val paramPtr: Variable = new Variable("param pointer", isPointer = true)

    /** Saves the `Reg.framePtr` of the caller so that it can be restored at the end of the procedure. */
    val dynamicLink: Variable = new Variable("dynamic link", isPointer = true)

    /** Saves the return address in the caller so that control can jump back to the caller at the end of
      * the procedure.
      */
    val savedPC: Variable = new Variable("saved pc")

    /** When this procedure is nested in another outer procedure, holds the address of the frame of the outer procedure
      * to enable access to the variables of the outer procedure.
      */
    val staticLink: Variable = new Variable("static link", isPointer = true)

    /** The label of the memory address at which the compiled code of this procedure will appear. */
    val label = new Label(s"procedure $name")

    /** The code of the body of the procedure. */
    var code: Code = null

    override def toString = s"Procedure($name)"
  }

  /** Code that calls a closure.
    *
    * @param closure is code that evaluates to the `Chunk` representing the closure
    * @param arguments are pieces of code that are evaluated to yield to arguments to be passed to the procedure
    * @param parameters are freshly-created variables that are used to create the `Chunk` that will be used to pass
    *                   the arguments to the procedure. They do not need to be the actual `parameters` from the
    *                   `Procedure` that will be invoked, since that is not known until runtime, but their `isPointer`
    *                   flag must match the `isPointer` flag of the corresponding parameters of the procedure that
    *                   will be invoked (this information can be derived from the static type of the closure).
    * @param isTail when true, the generated code will return from the called closure directly to the caller of the
    *               current procedure, skipping the current procedure on the return.
    *
    * It is always safe to leave `isTail` as `false`. `isTail` may be set to `true` if the `Call` is the last thing
    * that can happen in the current procedure.
    */
  case class CallClosure(closure: Code, arguments: Seq[Code], parameters: Seq[Variable], isTail: Boolean = false)
    extends Code

  /** Code that creates a closure from the given `procedure`. The generated code creates a `Chunk` containing a pointer
    * to the code of `procedure`, as well as a pointer to the current execution frame to allow the closure to close
    * over current variables. */
  case class Closure(procedure: Procedure) extends Code
}

