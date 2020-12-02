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
import CodeBuilders._
import Assembler._
import MemoryManagement._
import cs241e.mips._
import cs241e.Utils._
import Debugger._
import tools._

import scala.collection.mutable

/** Implementations of various transformations on the `Code` objects defined in `ProgramRepresentation.scala`.
  * In general, the transformations successively eliminate various types of `Code` objects by translating them
  * into sequences of simpler `Code`s, until only `Code`s directly representing machine instructions are left.
  */

object Transformations {
  /* ############################################################### */
  /* ## Assignment 2 ############################################### */
  /* ############################################################### */

  /* Before doing Assignment 2, read the code in the first half of `ProgramRepresentation.scala`
   * (up to the `Block` case class) to get an idea of how we will represent programs using the various subclasses
   * of the `Code` class.
   *
   * Hint: You can navigate to the `Code` class by Ctrl-clicking on any use of the word `Code`.
   */

  /* Complete the implementation of the following method by replacing the `???`. */

  /** Given a sequence of `Code`s that may be one of `CodeWord`, `Define`, `Use`, or `BeqBne`,
    * resolve all of the labels, and output the corresponding MIPS machine-language program
    * as a sequence of `Word`s.
    *
    * Refer to `ProgramRepresentation.scala` for documentation of the meanings of these `Code`s.
    *
    * If a label is defined multiple times or if a label is used but not defined, call
    * `sys.error()` with an appropriate error message.
    *
    * Scala hint: Consult the Scala library documentation for classes such as Seq:
    * https://www.scala-lang.org/api/current/scala/collection/immutable/Seq.html
    *
    * The high-level structure of this method is given for you, but you should learn about
    * methods such as `foreach` and `flatMap` because you may want to use them yourself in
    * later assignments.
    */
  def eliminateLabels(code: Seq[Code]): Seq[Word] = {
    val labelToValue = mutable.Map[Label, Int]()

    /* First pass: fill in the `labelToValue` map with an address for each label. */
    def setLabels(): Unit = {
      var location = 0
      code.foreach {
        case Define(label) => {
          if (labelToValue.contains(label)) {
            sys.error(label.toString + " has already been defined")
          }
          labelToValue.put(label, location)
        }
        case _ => {}
        location += 4
      }
    }

    /* Second pass: replace each `Code` with an equivalent sequence of `Word`s. */
    def translate: Seq[Word] = {
      var location = 0
      code.flatMap {
        case Define(label) => Seq()
        case CodeWord(word) => {
          location += 4
          Seq(word)
        }
        case Use(label) => {
          if (!labelToValue.contains(label)){
            sys.error(label.toString +" is undefined")

          }
          var offset = labelToValue.get(label).get
          location += 4
          Seq(Word(encodeSigned(offset.toLong)))
        }
        case BeqBne(bits, label) => {
          location += 4
          var offset = ((labelToValue.get(label).get - location)/4).toLong
          var seqbool = bits ++ encodeSigned(offset,16)
          Seq(Word(seqbool))
        }
        case _ => impossible(s"Encountered unsupported code $code.")
      }
    }
    setLabels()
    translate
  }

  /** Links two sequences of code together to form a single program. */
  def link(codes1: Seq[Code], codes2: Seq[Code]): Seq[Code] = codes1 ++ codes2

  /** Remove all `Comment`s from a sequence of `Code`s.
    *
    * Assumes that the input sequence does not contain any `Code`
    * types that are defined after `Comment` in `ProgramRepresentation.scala`.
    */
  def eliminateComments(codes: Seq[Code]): Seq[Code] = {
    def isNotComment(code: Code): Boolean = code match {
      case Comment(message:String) => false
      case _   => true
    }
    var eliminatedCodes = Seq[Code]()
    for (i<-codes){
      if (isNotComment(i)) eliminatedCodes = eliminatedCodes.appended(i)
    }
    return eliminatedCodes
  }

  /** Eliminate all `Block`s from a tree of `Code` by flattening the tree into a sequence of
    * `Code`s other than `Block`s.
    *
    * Assumes that the input `code` tree does not contain any `Code`
    * types that are defined after `Block` in `ProgramRepresentation.scala`.
    */
  def isBlock(code: Code): Boolean = code match {
    case Block(stmts) => true
    case _ => false
  }
  def eliminateBlocks(code: Code): Seq[Code] = code match {
    case Block(children) => {
      var ans = Seq[Code]()
      for (i <- children) {
        if (isBlock(i)) ans = ans ++ eliminateBlocks(i)
        else {
          ans = ans.appended(i)
        }
      }
      ans
    }
    case _ => Seq(code)
  }

  /** Transform a `Code` tree by applying the function `fun` to transform each node of the tree of type `Code`.
    *
    * More specifically, at a given node `code`, first recursively call `mapCodeTotal` to transform each of
    * the children of the node. Then apply `fun` on the resulting node whose children have been transformed.
    *
    * Note: `mapCodeTotal` should handle **all** the various possible subclasses of `Code`, not only the ones that
    * we have used so far.
    */
  def mapCodeTotal(code: Code, fun: Code=>Code): Code = {
    /** Apply `mapCodeTotal` on all child code nodes of the argument code node `code`. */
    def processChildren(code: Code): Code = code match {
      case Block(children) => Block(children.map((p => mapCodeTotal(p, fun))))
      case Scope(variables, body) => Scope(variables, mapCodeTotal(body,fun))
      case IfStmt(elseLabel, e1, comp, e2, thens, elses) => IfStmt(elseLabel, mapCodeTotal(e1,fun), comp, mapCodeTotal(e2,fun), mapCodeTotal(thens,fun), mapCodeTotal(elses,fun))
      case Call(procedure, args, isTail) => {
        Call(procedure, args.map(p => mapCodeTotal(p, fun)), isTail)
      }
      case CallClosure(closure, args, params, isTail) => CallClosure(mapCodeTotal(closure, fun),args.map(p => mapCodeTotal(p, fun)),params, isTail )
      case _ => code
    }

    fun(processChildren(code))
  }

  /** A adaptation of `mapCodeTotal` to transform code trees with a partial function. If the partial
    * function is not defined for a particular tree node, that tree node is left unmodified.
    *
    * Scala Hint: Read
    * https://www.scala-lang.org/api/current/scala/PartialFunction.html
    * for an explanation of `PartialFunction`.
    */
  def mapCode(code: Code, fun: PartialFunction[Code, Code]): Code = {
    mapCodeTotal(code, code => if(fun.isDefinedAt(code)) fun(code) else code)
  }

  /* ############################################################### */
  /* ## Assignment 3 ############################################### */
  /* ############################################################### */

  /** Eliminate all `VarAccess`es from a tree of `Code` by replacing them with machine language code for
    * reading or writing the relevant variable.
    *
    * To do this, we need an activation record or `frame` containing the `Variable`s used in `code` that
    * determines the address in memory where each `Variable` is stored, as an offset from the address in
    * `Reg.framePointer`. See the definition of the `Chunk` class in `MemoryManagement.scala` for details.
    *
    * Hint: the `mapCode` method is very helpful for transforming `Code` in general, and for eliminating
    * `VarAccess`es specifically.
    *
    * Scala Hint: {case va: VarAccess => ???} defines a `PartialFunction` that is defined for `VarAccess`es
    * but is not defined for any other type of `Code`.
    *
    * Assumes that the input sequence does not contain any `Code`
    * types that are defined after `VarAccess` in `ProgramRepresentation.scala`.
    *
    * The code generated to implement a variable access may modify the value of Reg.scratch.
    * If the access is a read, it may also of course modify the target register. If you need more
    * than these registers, you may add new scratch registers to Reg.scala. The generated code must
    * not modify the values of any other registers that are already listed in Reg.scala.
    */
  def eliminateVarAccessesA3(code: Code, frame: Chunk): Code = mapCode(code,
      {case va: VarAccess => {
        if (!va.read) frame.store(Reg.framePointer, va.variable, va.register)
        else frame.load(Reg.framePointer, va.register, va.variable)
      }})

  /** Given a `body` of `Code` and a `frame` of variables that it uses, generates
    * code that allocates space for the `frame` on the stack and sets `Reg.framePointer` to
    * point to it, followed by the `body`, followed by code to free the space for the `frame` from the stack.
    */
  def allocateFrameOnStack(body: Code, frame: Chunk): Code =
    block(
      Comment("allocate on stack"),
      Stack.allocate(frame),
      ADD(Reg.framePointer, Reg.stackPointer, Reg.zero),
      body,
      Stack.pop
      )

  /** A bundle of machine language code in the form of a sequence of `Word`s and a `debugTable` for the `Debugger`. */
  case class MachineCode(words: Seq[Word], debugTable: DebugTable)

  /** Given a `Code` tree containing only `Code` types that are defined before `Block` in `ProgramRepresentation.scala`,
    * successively eliminates all `Code` types to yield just a sequence of `Word`s representing the equivalent program
    * in machine language. In addition, generates a `DebugTable` for that program.
    */
  def toMachineCode(code: Code): MachineCode = {
    val code2 = eliminateBlocks(code)
    val debugTable = createDebugTable(code2)
    val code3 = eliminateComments(code2)
    val code4 = eliminateLabels(code3)
    MachineCode(code4, debugTable)
  }

  /** Given a `Code` tree that may contain any `Code` types defined in `ProgramRepresentation.scala` up to and
    * including `VarAccess`, successively eliminates all `Code` types to yield just a sequence of `Word`s
    * representing the equivalent program in machine language. In addition, generates a `DebugTable` for that program.
    *
    * Requires a sequence of all `Variable`s that are accessed within `code`. The generated machine language code
    * will allocate space for the variables on the stack and free it at the end.
    */
  def compilerA3(code: Code, variables: Seq[Variable]): MachineCode = {
    val frame = Chunk(variables)
    val code1 = eliminateVarAccessesA3(code, frame)
    val code2 = allocateFrameOnStack(code1, frame)
    toMachineCode(block(code2, JR(Reg.link)))
  }

  /* ############################################################### */
  /* ## Assignment 4 ############################################### */
  /* ############################################################### */

  /** Eliminate all `Scope`s from a tree of `Code` by simply returning the `code` field
    * for each one.
    *
    * Return a pair of the resulting code and a sequence of the temporary `Variable`s extracted
    * from the `Scope`s.
    *
    * Assumes that the input `code` tree does not contain any `Code`
    * types that are defined after `Scope` in `ProgramRepresentation.scala`.
    *
    * Hint: Use `mapCode`.
    */
  def eliminateScopes(code: Code): (Code, Seq[Variable]) = {
    var vars = Seq[Variable]()
    val ans = mapCode(code,
    {case scope: Scope =>{
      vars = vars ++ scope.variables
      scope.code
    }})
    (ans, vars)
  }

  /** Eliminate all `IfStmt`s from a tree of `Code` by translating them to simpler pieces
    * of `Code`.
    *
    * Assumes that the input `code` tree does not contain any `Code`
    * types that are defined after `IfStmt` in `ProgramRepresentation.scala`.
    *
    * The code generated to implement an if statement may modify the values of Reg.result and Reg.scratch. If you
    * need more than these registers, you may add new scratch registers to Reg.scala. The generated code
    * must not modify the values of any other registers that are already listed in Reg.scala.
    */
  def isIfstmt(code: Code): Boolean = code match {
    case IfStmt(label, code1, code2, code3, code4, code5) => true
    case Scope(variables, body) => true
    case Block(children) => true
    case _ => false
  }
  def eliminateIfStmtsHelper(code: Code): Code = code match {
    case IfStmt(label, e1, comp, e2, thens, elses) => {
      var ans = Seq[Code](Comment("begin ifstmt"))
      val temp = new Variable("temp")
      val endLabel = new Label("endLabel")
      if (isIfstmt(e1)) ans = ans ++ Seq(eliminateIfStmts(e1), write(temp, Reg.result))
      else {
        ans = ans ++ Seq(e1, write(temp, Reg.result))
      }
      if (isIfstmt(e2)) ans = ans ++ Seq(eliminateIfStmts(e2), read(Reg.scratch,temp), comp)
      else {
        ans = ans ++ Seq(e2, read(Reg.scratch, temp), comp)
      }
      if (isIfstmt(thens)) ans = ans ++ Seq(eliminateIfStmts(thens), beq(Reg.zero, Reg.zero, endLabel), Define(label))
      else {
        ans = ans ++ Seq(thens, beq(Reg.zero, Reg.zero, endLabel), Define(label))
      }
      if (isIfstmt(elses)) ans = ans ++ Seq(eliminateIfStmts(elses), Define(endLabel))
      else {
        ans = ans ++ Seq(elses, Define(endLabel))
      }
      Scope(Seq(temp),Block(ans))
    }
    case _ => code
  }



  def eliminateIfStmts(code: Code): Code = code match {
    case IfStmt(label, e1, comp, e2, thens, elses) => eliminateIfStmtsHelper(code)
    case Scope(variables, body) => Scope(variables, eliminateIfStmts(body))
    case Block(children) => Block(children.map(eliminateIfStmts))
    case _ => code
  }

  def compilerA4(code: Code): MachineCode = {
    val code1 = eliminateIfStmts(code)
    val (code2, variables) = eliminateScopes(code1)
    assert(variables.distinct == variables)
    compilerA3(code2, variables)
  }

  /* ############################################################### */
  /* ## Assignment 5 ############################################### */
  /* ############################################################### */

  /** For each `Variable` in the sequence `params`, creates a new variable (intended to be used as a
    * temporary variable). Returns a sequence of temporary variables of the same length as the input
    * sequence `params`. For each `Variable` in `params`, if the `isPointer` flag of the variable is
    * set to true, then the `isPointer` flag of the temporary variable at the same position in the returned
    * sequence will also be set to true.
    */
  def createTempVars(params: Seq[Variable]): Seq[Variable] =
    params.map(param => new Variable("temp", param.isPointer))

  /** Given a set of `keys` for a map and a `function`, applies the function to each key, and stores
    * the result in a `Map` from `keys` to the `function` values.
    */
  def makeMap[A, B](keys: Seq[A], function: A=>B): Map[A, B] = keys.map(key => (key, function(key))).toMap

  /** Given a sequence of `Procedure`s, compiles the procedures to machine language. The first procedure
    * in the sequence is considered the main procedure that should be executed first in the program. This
    * is achieved by adding an extra `startProcedure` that calls the main procedure with the values
    * of registers $1 and $2 as arguments. The main procedure must have exactly two parameters to receive
    * these values.
    */
  def compilerA5(inputProcedures: Seq[Procedure]): MachineCode = {
    require(!inputProcedures.isEmpty)
    require(inputProcedures.head.parameters.size == 2)

    val procedures = startProcedure(inputProcedures.head) +: inputProcedures
    /** The `Chunk` to store the parameters of each procedure. */
    val paramChunks: Map[Procedure, Chunk] =
      makeMap(procedures, procedure => Chunk(procedure.parameters))

    /** Compile a single `procedure` to machine language. */
    def compileProcedure(procedure: Procedure): Code = {

      /** Eliminate all `Call`s from a tree of `Code` by translating them to simpler pieces
        * of `Code`.
        *
        * The general strategy for passing arguments is as follows:
        * - create temporary variables, one for each argument
        * - evaluate the arguments, storing them in the temporary variables
        * - allocate memory for the parameter `Chunk`
        * - copy the values of the temporary variables to the parameter `Chunk`
        *
        * Assumes that the input `code` tree does not contain any `Code`
        * types that are defined after `Call` in `ProgramRepresentation.scala`.
        */
      def eliminateCalls(code: Code): Code = mapCode(code, {
        case call: Call => {
          val tempVars = createTempVars(call.procedure.parameters)
          val len = call.arguments.length
          var storeCode = Seq[Code]()
          var copyCode = Seq[Code]()
          for (i <- 0 to len-1){
            storeCode = storeCode.appended(assign(tempVars(i), call.arguments(i)))
          }
          val parameters = createTempVars(tempVars)
          val chunk = Chunk(parameters)
          for (i <- 0 to len-1){
            copyCode = copyCode ++ Seq(
              read(Reg.scratch, tempVars(i)),
              chunk.store(Reg.result, parameters(i), Reg.scratch)
            )
          }
          Scope(tempVars, block(
            Block(storeCode),
            Stack.allocate(Chunk(parameters)),
            Block(copyCode),
            LIS(Reg.targetPC),
            Use(call.procedure.label),
            JALR(Reg.targetPC)
          ))
        }
      })

      /** Adds a prologue and epilogue to the `code` of a procedure.
        *
        * The prologue assumes that when the procedure is called, `Reg.result` contains the address
        * of the parameter chunk for the procedure.
        *
        * The prologue:
        * - saves the address of the parameter chunk in order to later store it into the `paramPtr` variable
        *   in the procedure `frame`
        * - allocates space for the procedure `frame` on the stack
        * - saves the caller's value of `Reg.framePointer` in the `dynamicLink` variable of the procedure `frame`
        * - sets `Reg.framePointer` to the address of the newly-allocated `frame` for the current procedure
        * - saves `Reg.link` in the `savedPC` variable of the `frame`
        * - stores the saved address of the parameter chunk into the `paramPtr` variable in the procedure `frame`
        *
        * The epilogue:
        * - restores `Reg.link` and `Reg.framePointer` from the current frame
        * - pops the current procedure's paramChunk and `frame` off the stack
        * - returns control to the caller
        *
        * Warning: this method transforms code after `eliminateVarAccesses` has been called. Therefore, this method
        * should not introduce any new `VarAccess`es into the code (by calling `read` or `write`).
        */
      def addEntryExit(code: Code, frame: Chunk): Code = {

        val enter = block(
          ADD(Reg.savedParamPtr, Reg.result),
          Stack.allocate(frame),
          frame.store(Reg.result, procedure.dynamicLink, Reg.framePointer),
          ADD(Reg.framePointer, Reg.result),
          frame.store(Reg.framePointer, procedure.savedPC, Reg.link),
          frame.store(Reg.framePointer, procedure.paramPtr, Reg.savedParamPtr)
        )
        val exit = block(
          frame.load(Reg.framePointer, Reg.link, procedure.savedPC),
          frame.load(Reg.framePointer, Reg.framePointer, procedure.dynamicLink),
          Stack.pop,
          Stack.pop,
          JR(Reg.link)
        )
        block(Define(procedure.label), enter, code, exit)
      }

      /** Eliminate all `VarAccess`es from a tree of `Code` by replacing them with machine language code for
        * reading or writing the relevant variable.
        *
        * In contrast to Assignment 3, this method needs to handle accesses not only to variables,
        * but also to the parameters of the current procedure.
        *
        * Assumes that the input sequence does not contain any `Code`
        * types that are defined after `VarAccess` in `ProgramRepresentation.scala`.
        *
        * The code generated to implement a variable access may modify the value of Reg.scratch.
        * If the access is a read, it may also of course modify the target register. If you need more
        * than these registers, you may add new scratch registers to Reg.scala. The generated code must
        * not modify the values of any other registers that are already listed in Reg.scala.
        */
      def eliminateVarAccessesA5(code: Code, frame: Chunk): Code = mapCode(code,
        {case va: VarAccess => {
          if (frame.variables.contains(va.variable)) eliminateVarAccessesA3(va, frame)
          else {
            if (va.read) block(
            frame.load(Reg.framePointer, Reg.scratch, procedure.paramPtr),
            paramChunks.get(procedure).get.load(Reg.scratch, va.register, va.variable)
          )
            else block(
            frame.load(Reg.framePointer, Reg.scratch, procedure.paramPtr),
            paramChunks.get(procedure).get.store(Reg.scratch, va.variable, va.register)
          )
          }
        }})

      /* Main body of compileProcedure. */

      val code1 = eliminateCalls(procedure.code)
      val code2 = eliminateIfStmts(code1)
      val (code3, variables) = eliminateScopes(code2)
      assert(variables.distinct == variables)

      val frame = Chunk(variables ++ Seq(procedure.dynamicLink, procedure.paramPtr, procedure.savedPC))

      val code4 = eliminateVarAccessesA5(code3, frame)
      addEntryExit(code4, frame)
    }

    /* Main body of compilerA5. */

    val code = block(
      Stack.allocate(Chunk(procedures.head.parameters)), // allocate parameter chunk for start procedure
      Block(procedures.map(compileProcedure))
    )
    toMachineCode(code)
  }

  def startProcedure(mainProcedure: Procedure): Procedure = {
    val ret = new Procedure("start", Seq())
    ret.code = Call(mainProcedure, Seq(ADD(Reg.result, Reg.input1), ADD(Reg.result, Reg.input2)))
    ret
  }

  /* ############################################################### */
  /* ## Assignment 6 ############################################### */
  /* ############################################################### */

  val closureCode = new Variable("closure code")
  val closureEnvironment = new Variable("closure environment", isPointer = true)
  /** A chunk representing a closure consisting of:
    * - the address of the code of the closure
    * - the address of the frame of the enclosing environment of the closure, which will become the static link when
    *   the closure is invoked
    */
  lazy val closureChunk = Chunk(Seq(closureCode, closureEnvironment))

  /** Given a sequence of `Procedure`s, compiles the procedures to machine language. The first procedure
    * in the sequence is considered the main procedure that should be executed first in the program. This
    * is achieved by adding an extra `startProcedure` that calls the main procedure with the values
    * of registers $1 and $2 as arguments. The main procedure must have exactly two parameters to receive
    * these values.
    */
  def compilerA6(inputProcedures: Seq[Procedure]): MachineCode = {
    require(!inputProcedures.isEmpty)
    require(inputProcedures.head.parameters.size == 2)

    val procedures = startProcedure(inputProcedures.head) +: inputProcedures

    /** The `Chunk` to store the parameters and static link of each procedure. */
    val paramChunks: Map[Procedure, Chunk] = makeMap(procedures, procedure => Chunk(procedure.parameters :+ procedure.staticLink))

    /** The set of procedures whose frame needs to be allocated on the heap (instead of on the stack).
      * This includes:
      * - every procedure that is ever made into a closure
      * - recursively, every enclosing procedure that contains an inner procedure nested within it whose frame is
      *   allocated on the heap
      */
    val frameOnHeap: Set[Procedure] = {
      var ans = Set[Procedure]()

      for (i <- procedures){
        mapCode(i.code, {case closure: Closure =>{
          ans = ans.+(closure.procedure)
          closure
        }})
      }

      val tempans = ans
      for (i <- tempans){
        var curr = i
        while(curr.outer != None){
          curr = curr.outer.get
          ans = ans.+(curr)
        }
      }

      ans
    }

    

    /** The first phase of compilation: performs the transformations up to eliminateScopes so that
      * the full set of variables of the procedure is known, and so that a `Chunk` can be created for the
      * frame of the procedure. Since the second phase requires a frame for every
      * procedure, the first phase must be completed for all the procedures before the second phase
      * can begin. Returns the code for the procedure and the `Chunk` for the procedure's frame.
      */
    def phaseOne(procedure: Procedure): (Code, Chunk) = {

      /** Generates the code that computes the value for the static link in a call to target.
        * Specifically, the static link should be the address of the frame in the current chain
        * of static links that corresponds to the procedure that directly encloses target.
        * This address should be placed in `Reg.result`.
        *
        * When `target` is a top-level procedure with no outer enclosing procedure, returns code that
        * places the zero word in `Reg.result`.
        */
      def computeStaticLink(target: Procedure): Code = {
        if (target.outer == None) const(0)
        else if (procedure.depth-target.depth == -1) block(
          Comment("I am computing staticLink"),
          ADD(Reg.result, Reg.framePointer)
        )
        else {
          var ans = Seq[Code](Comment("I am computing staticLink"),ADD(Reg.scratch, Reg.framePointer))
          var curr = procedure
          var chunk = Chunk( Seq(curr.dynamicLink, curr.paramPtr, curr.savedPC) )
          while(curr.depth - target.depth > -1){
            ans = ans ++ Seq(
              chunk.load(Reg.scratch, Reg.scratch,curr.paramPtr),
              paramChunks(curr).load(Reg.scratch, Reg.result, curr.staticLink),
              ADD(Reg.scratch, Reg.result)
            )
            curr = curr.outer.get
            chunk = Chunk(  Seq(curr.dynamicLink, curr.paramPtr, curr.savedPC))
          }
          ans = ans.appended(Comment("Completing computing staticLink"))
          Block(ans)
        }
      }

      /** Eliminate all `Call`s and `CallClosure`s from a tree of `Code` by translating them to simpler pieces
        * of `Code`.
        *
        * Assumes that the input `code` tree does not contain any `Code`
        * types that are defined after `CallClosure` in `ProgramRepresentation.scala`.
        */

      def eliminateCalls(code: Code): Code = {

        /** Generates the implementation of a procedure call (either direct or through a closure).
          * The general strategy is as follows:
          * - compute arguments (including static link) and allocate parameter chunk as in Assignment 5
          * - if this is a tail call
          *   - set Reg.link and Reg.framePtr to the values from the current procedure's caller like
          *       in the epilogue in addEntryExit
          *   - if the target parameter chunk is on the heap
          *     - deallocate frame and parameter chunk of current procedure (if they are on the stack)
          *   - else
          *     - deallocate target parameter chunk and frame and parameter chunk of current procedure
          *         (if they are on the stack)
          *     - allocate new target parameter chunk
          *     - copy the deallocated target parameter chunk to the new target parameter chunk
          *   - endif
          * - endif
          * - transfer control to target procedure
          *
          * Hint: be careful about which base registers hold the address of the various `Chunk`s, particularly
          * the frame of the current procedure and the parameter `Chunk` of the target procedure.
          *
          * @param targetPC code to compute the address of the target procedure to be called and put it in
          *                 `Reg.targetPC`
          * @param args code to compute the arguments and static link to be passed to the target procedure
          * @param paramChunk the Chunk to hold the arguments to be passed to the target procedure
          * @param targetOnHeap true if the paramChunk of the target procedure is to be allocated on the heap
          *                     instead of the stack
          * @param isTail true if the call is in tail position in the calling procedure
          */
        def implementCall( targetPC: Code,
                           args: Seq[Code],
                           paramChunk: Chunk,
                           targetOnHeap: Boolean,
                           isTail: Boolean
                           ): Code = {
          val tempVars = createTempVars(paramChunk.variables)
          val len = args.length
          var storeCode = Seq[Code](Comment("The procedure i am calling is "+ procedure.name))
          var copyCode = Seq[Code](Comment("The procedure i am calling is "+ procedure.name))
          for (i <- 0 to len-1){
            storeCode = storeCode.appended(assign(tempVars(i), args(i)))
          }
          for (i <- 0 to len) {
            copyCode = copyCode ++ Seq(
              Comment("~~I am copying " + tempVars(i).name),
              read(Reg.scratch, tempVars(i)),
              paramChunk.store(Reg.result, paramChunk.variables(i), Reg.scratch)
            )
          }

          if (isTail){
            val codeP1 = Seq[Code](
              Comment(procedure.name+ " is a tail call!"),
              Block(storeCode),
              targetPC,
              write(tempVars(tempVars.length-1), Reg.result)
            )
            var codeP2 = Seq[Code]()
            if (targetOnHeap) {
              codeP2 = codeP1 ++ Seq(
                heap.allocate(paramChunk),
              ) ++ copyCode ++ Seq(
                read(Reg.link, procedure.savedPC),
                read(Reg.framePointer, procedure.dynamicLink)
              )
              if (!frameOnHeap.contains(procedure)){
                codeP2 = codeP2 ++ Seq(
                  Stack.pop,
                  Stack.pop
                )
              }
              codeP2 = codeP2.appended(JR(Reg.targetPC))
            } else {
              codeP2 = codeP1 ++ Seq(
                Stack.allocate(paramChunk),
              ) ++ copyCode ++ Seq(
                read(Reg.link, procedure.savedPC),
                read(Reg.framePointer, procedure.dynamicLink)
              )
              if (!frameOnHeap.contains(procedure)) {
                codeP2 = codeP2 ++ Seq(
                  Stack.pop,
                  Stack.pop,
                  Stack.pop,
                  ADD(Reg.scratchExtend1, Reg.result),
                  Stack.allocate(paramChunk),
                  copyChunk(Reg.result, Reg.scratchExtend1),
                  JR(Reg.targetPC)
                )
              } else {
                codeP2 = codeP2 ++ Seq(
                  JR(Reg.targetPC)
                )
              }

            }
            Scope(tempVars, Block(
              codeP2
            ))
          }else {
            /** case2 it is not a tail call*/
            val codeP1 = Seq[Code](
              Block(storeCode),
              targetPC,
              write(tempVars(tempVars.length-1), Reg.result)
            )
            val codeP2 = Seq[Code](
              Block(copyCode),
              JALR(Reg.targetPC)
            )
            if (targetOnHeap){
                Scope(tempVars, Block(
                codeP1 ++ Seq(heap.allocate(paramChunk))++codeP2
              ))
            } else {
                Scope(tempVars, Block(
                codeP1 ++ Seq(Stack.allocate(paramChunk))++codeP2
              ))
            }
          }
        }

        mapCode(code, {
          case call: Call =>{
            val pcAndStaticCode = block(
              computeStaticLink(call.procedure),
              LIS(Reg.targetPC),
              Use(call.procedure.label)
            )
            implementCall(pcAndStaticCode, call.arguments, paramChunks(call.procedure),frameOnHeap.contains(call.procedure), call.isTail)
          }
          case callClosure: CallClosure =>{
            val pcAndStaticCode = block(
              Comment("I am calling closure"),
              callClosure.closure,
              closureChunk.load(Reg.result, Reg.targetPC, closureCode),
              closureChunk.load(Reg.result, Reg.result, closureEnvironment)
            )
            implementCall(pcAndStaticCode, callClosure.arguments, Chunk(callClosure.parameters :+ new Variable("tempstatic", true)),true, callClosure.isTail)
          }
        })
      }

      /** Eliminate all `Closure`s from a tree of `Code` by translating them to simpler pieces of `Code`.
        *
        * As given, this method just returns the `code` unchanged. When you implement handling of closures in
        * Assignment 6, you will change the method body to actually eliminate `Closure`s.
        *
        **/
      def eliminateClosures(code: Code): Code = mapCode(code, {case closure: Closure =>
        block(
          Comment("I am allocating frame for closureChunk " + closure.procedure.name),
          heap.allocate(closureChunk),
          LIS(Reg.scratchExtend4),
          Use(closure.procedure.label),
          closureChunk.store(Reg.result, closureCode, Reg.scratchExtend4),
          ADD(Reg.scratchExtend4, Reg.result),
          computeStaticLink(closure.procedure),
          closureChunk.store(Reg.scratchExtend4, closureEnvironment, Reg.result),
          ADD(Reg.result, Reg.scratchExtend4),
          Comment("End eliminate closure")
        )
      })

      /** Find `Call`s that appear in tail position in their containing procedure. Replace each one with the same
        * `Call` but with `isTail` set to `true`.
        *
        * Hint: If a call in tail position is to a procedure nested within the current one, is it safe to do
        * a tail call?
        *
        * As given, this method just returns the `code` unchanged. When you implement handling of tail calls in
        * Assignment 6, you will change the method body to actually detect tail calls.
        */
      def isOutter(call: Call): Boolean = {
        if (call.procedure.outer == None)  return false
        var curr = call.procedure.outer
        while (curr != None){
          if (curr.get == procedure) return true
          curr = curr.get.outer
        }
         false
      }
      def detectTailCalls(code: Code): Code = code match {

        case call: CallClosure => CallClosure(call.closure, call.arguments, call.parameters, true)
        case call : Call if (call.procedure.outer == None || !isOutter(call)) => {
          Call(call.procedure, call.arguments, true)
        }
        case Block(children) =>{
          val tail = detectTailCalls(children(children.size-1))
          Block(children.updated(children.size-1, tail))
        }
        case IfStmt(elseLabel, e1, comp, e2, thens, elses) => {
          IfStmt(elseLabel, e1, comp, e2, detectTailCalls(thens), detectTailCalls(elses))
        }
        case Scope(variables, code) => Scope(variables, detectTailCalls(code))
        case _ => code

      }

      /* Main body of phaseOne. */

      val code1 = eliminateClosures(procedure.code)
      //val code2 = detectTailCalls(code1)
      val code3 = eliminateCalls(code1)
      val code4 = eliminateIfStmts(code3)
      val (code5, variables) = eliminateScopes(code4)
      assert(variables.distinct == variables)

      val frame = Chunk(Seq( procedure.dynamicLink,procedure.paramPtr, procedure.savedPC) ++ variables)

      (code5, frame)
    }

    val phaseOneResults: Map[Procedure, (Code, Chunk)] = makeMap(procedures, phaseOne)

    def phaseTwo(procedure: Procedure): Code = {
      val (code, frame) = phaseOneResults(procedure)

      /** Adds a prologue and epilogue to the `code` of a procedure.
        *
        * Hint: The implementation of this method starts out the same as in Assignment 5, and you can copy
        * your code from there. When you implement closures, you need to modify it so that the frame
        * and parameters are on the heap if `frameOnHeap(procedure)` is true.
        *
        * The prologue assumes that when the procedure is called, `Reg.result` contains the address
        * of the parameter chunk for the procedure.
        *
        * The prologue:
        * - saves the address of the parameter chunk to stores it into the `paramPtr` variable in the procedure `frame`
        * - allocates space for the procedure `frame` on the stack or heap
        * - saves the caller's value of `Reg.framePointer` in the `dynamicLink` variable of the procedure `frame`
        * - sets `Reg.framePointer` to the address of the newly-allocated `frame` for the current procedure
        * - saves `Reg.link` in the `savedPC` variable of the `frame`
        *
        * The epilogue:
        * - restores `Reg.link` and `Reg.framePointer` from the current frame
        * - pops the current procedure's paramChunk and `frame` off the stack if they are allocated on the stack
        * - returns control to the caller
        *
        * Warning: this method transforms code after `eliminateVarAccesses` has been called. Therefore, this method
        * should not introduce any new `VarAccess`es into the code (by calling `read` or `write`).
        */
      def addEntryExit(code: Code, frame: Chunk): Code = {
        var enter = block()
        var exit = block()
        if (frameOnHeap.contains(procedure)){
          enter = block(
            ADD(Reg.savedParamPtr, Reg.result),
            Stack.allocate(frame),
            ADD(Reg.tempAddEntry,Reg.result),
            frame.store(Reg.result, procedure.dynamicLink, Reg.framePointer),
            frame.store(Reg.result, procedure.savedPC, Reg.link),
            frame.store(Reg.result, procedure.paramPtr, Reg.savedParamPtr),
            heap.allocate(frame),
            copyChunk(Reg.result, Reg.tempAddEntry),
            Stack.pop,
            ADD(Reg.framePointer, Reg.result)
          )
          exit = block(
            Comment(procedure.name + " is exiting"),
            frame.load(Reg.framePointer, Reg.link, procedure.savedPC),
            frame.load(Reg.framePointer, Reg.framePointer, procedure.dynamicLink),
            JR(Reg.link)
          )
        }
        else {
           enter = block(
            ADD(Reg.savedParamPtr, Reg.result),
            Stack.allocate(frame),
            frame.store(Reg.result, procedure.dynamicLink, Reg.framePointer),
            ADD(Reg.framePointer, Reg.result),
            frame.store(Reg.framePointer, procedure.savedPC, Reg.link),
            frame.store(Reg.framePointer, procedure.paramPtr, Reg.savedParamPtr)
          )
           exit = block(
             Comment(procedure.name + " is exiting"),
            frame.load(Reg.framePointer, Reg.link, procedure.savedPC),
            frame.load(Reg.framePointer, Reg.framePointer, procedure.dynamicLink),
            Stack.pop,
            Stack.pop,
            JR(Reg.link)
          )
        }
        block(Define(procedure.label), enter, code, exit)
      }

      /** Eliminate all `VarAccess`es from a tree of `Code` by replacing them with machine language code for
        * reading or writing the relevant variable.
        *
        * In contrast to Assignment 5, this method handles accesses to variables (and parameters) outside the currently
        * executing procedure, but in one of the outer procedures within which the current procedure is
        * nested. To do this, look up the chain of static links to find the frame (and its associated parameters)
        * of the procedure in which the variable is defined.
        *
        * Assumes that the input sequence does not contain any `Code`
        * types that are defined after `VarAccess` in `ProgramRepresentation.scala`.
        *
        * The code generated to implement a variable access may modify the value of Reg.scratch.
        * If the access is a read, it may also of course modify the target register. If you need more
        * than these registers, you may add new scratch registers to Reg.scala. The generated code must
        * not modify the values of any other registers that are already listed in Reg.scala.
        *
        * An error that students often make when implementing this method is to assume that the staticLink
        * and paramPtr are at constant offsets from the beginning of a chunk. The offsets change depending
        * on how many variables and parameters a given function has. Do not assume that the offsets are constant.
        * Instead, use `Chunk.read` and `Chunk.write` with the appropriate `Variable`s to access these values
        * in the chunk.
        */
      def eliminateVarAccesses(code: Code): Code = mapCode(code,
        {case va: VarAccess => {

          if (frame.variables.contains(va.variable)) eliminateVarAccessesA3(va, frame) //case1: var in the current frame
           else {
              var ans = Seq[Code](Comment("i am accessing " + va.variable.name),ADD(Reg.scratch, Reg.framePointer))
              var curr = procedure
              while (!phaseOneResults(curr)._2.variables.contains(va.variable) && !paramChunks(curr).variables.contains(va.variable)){
                ans  = ans ++ Seq(
                  Comment(va.variable.name + " is not in " + curr.name),
                  phaseOneResults(curr)._2.load(Reg.scratch,Reg.scratch, curr.paramPtr),
                  paramChunks(curr).load(Reg.scratch,Reg.scratch, curr.staticLink)
                )
                curr = curr.outer.get
              }
              if (phaseOneResults(curr)._2.variables.contains(va.variable)) {
                ans = ans.appended(Comment("found var " + va.variable.name +" at " + curr.name))
                if (va.read) ans = ans.appended(phaseOneResults(curr)._2.load(Reg.scratch, va.register, va.variable))
                else ans = ans.appended(phaseOneResults(curr)._2.store(Reg.scratch, va.variable, va.register))
                Block(ans)
              } else {
                ans = ans.appended(Comment("found var " + va.variable.name +" at " + curr.name))
                ans = ans.appended(phaseOneResults(curr)._2.load(Reg.scratch, Reg.scratch, curr.paramPtr))
                if (va.read) ans = ans.appended(paramChunks(curr).load(Reg.scratch, va.register, va.variable))
                else ans = ans.appended(paramChunks(curr).store(Reg.scratch, va.variable, va.register))
                Block(ans)
              }
          }
        }})

      /* Main body of phaseTwo. */

      val code1 = eliminateVarAccesses(code)
      val code2 = addEntryExit(code1, frame)
      code2
    }

    /* Main body of compilerA6. */

    val code = block(
      heap.initCode,
      Stack.allocate(Chunk(procedures.head.parameters :+ procedures.head.staticLink)), // allocate parameter chunk for start procedure
      Block(procedures.map(phaseTwo))
    )
    toMachineCode(code)
  }

}

