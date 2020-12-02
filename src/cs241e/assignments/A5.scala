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
import cs241e.assignments.tools._
import cs241e.assignments.Assembler._

object A5 {
  /** The code of `printInteger` from Assignment 4 encapsulated as a `Procedure`. The procedure should have
    * exactly one parameter, the integer to be printed. */
  lazy val printProcedure: Procedure = {
    val num = new Variable("num")
    val procedure = new Procedure("printProcedure", Seq(num))
    procedure.code = block(
      read(Reg.input1, num),
      A4.printIntegerCode
    )
    procedure
  }

  /** This procedure will be executed with an array of 32-bit integers as in Assignment 4.
    * It should take two parameters: the first is the address of the beginning of the array
    * and the second is the number of elements in the array.
    * The procedure should call `printProcedure` for each integer in the array in turn,
    * to print all the integers, and return.
    *
    * Test this procedure by compiling it with `printProcedure` and running it on various arrays.
    */
  lazy val printArray: Procedure = {
    val arr = new Variable("array")
    val len = new Variable("length")
    val tempLen = new Variable("templen")
    val procedure = new Procedure("printArray", Seq(arr, len))
    procedure.code = Scope(Seq(tempLen), block(
      whileLoop(read(Reg.result, tempLen), ltCmp, read(Reg.result, len),
      block(
        binOp(read(Reg.result, arr), plus, binOp(read(Reg.result, tempLen), times, const(4))),
        call(printProcedure, LW(Reg.result, 0, Reg.result)),
        incVar(tempLen)
      ))
    )
    )
    procedure
  }

  /** This procedure will be executed with an array of 32-bit integers as in Assignment 4.
    * It should take two parameters: the first is the address of the beginning of the array
    * and the second is the number of elements in the array.
    *
    * You may use multiple procedures if you wish. Generate them and return them in a `Seq`.
    * The tests will execute the first procedure in the sequence.
    *
    * The task is to determine the height of a binary tree and return it (in `Reg.result`).
    * Assume that every tree contains at least one node and hence has a height of at least one.
    * Each node of the tree is encoded in three consecutive elements (words) of the array:
    * a two's-complement integer stored at the node, the node's left child, and the node's right child.
    * Each child is specified as the array index of the first element of the child node.
    * The integer -1 indicates that a node does not have a left or right child. For example, the following tree:
    *
    *   77
    *  /  \
    * 22    -8
    *     /  \
    *   -36   999
    *
    * could be encoded by following array:
    *
    * A[0] = 77
    * A[1] = 3
    * A[2] = 6
    * A[3] = 22
    * A[4] = -1
    * A[5] = -1
    * A[6] = -8
    * A[7] = 9
    * A[8] = 12
    * A[9] = -36
    * A[10] = -1
    * A[11] = -1
    * A[12] = 999
    * A[13] = -1
    * A[14] = -1
    *
    * in which the root is encoded by the elements A[0], A[1] and A[2], the root's left child is encoded
    * by the elements A[3], A[4] and A[5], the root's right child is encoded by the elements A[6], A[7] and A[8],
    * the root's left-most grandchild is encoded by the elements A[9], A[10] and A[11],
    * and the root's right-most grandchild is encoded by the elements A[12], A[13] and A[14].
    *
    * This example tree has height 3.
    */
  lazy val treeHeight: Seq[Procedure] = {
    val originalArr = new Variable("originalArr")
    val arr = new Variable("array")
    val len = new Variable("length")
    val curr = new Variable("curr")
    val main = new Procedure("main", Seq(originalArr, len))
    val getHeight = new Procedure("getHeight", Seq(arr, curr))
    val currLeftIndex = new Variable("currli")
    val currRightIndex = new Variable("currri")
    val tempLen = new Variable("tempLen")
    val getLeftHeightCode = binOp(call(getHeight, read(Reg.result, arr), read(Reg.result, currLeftIndex)), plus, const(1))
    val getRightHeightCode = binOp(call(getHeight, read(Reg.result, arr), read(Reg.result, currRightIndex)), plus, const(1))
    main.code = call(getHeight, read(Reg.result, originalArr), const(0))
    getHeight.code = Scope(Seq(currLeftIndex, currRightIndex, tempLen), block(
        assign(currLeftIndex, block(
          assign(tempLen, binOp(read(Reg.result, curr), plus, const(1))),
          getArrVal(arr, tempLen)
        )),
      assign(currRightIndex, block(
        assign(tempLen, binOp(read(Reg.result, curr), plus, const(2))),
        getArrVal(arr, tempLen)
      )),
      ifStmt(read(Reg.result, currLeftIndex), eqCmp, const(-1), ifStmt(
        read(Reg.result, currRightIndex), eqCmp, const(-1), const(1), binOp(call(getHeight, read(Reg.result, arr), read(Reg.result, currRightIndex)), plus, const(1))
      ),
      ifStmt(read(Reg.result, currRightIndex), eqCmp, const(-1), binOp(call(getHeight, read(Reg.result, arr), read(Reg.result, currLeftIndex)), plus, const(1)),block(
        Comment("i am getting max"),
        maximum(binOp(call(getHeight, read(Reg.result, arr), read(Reg.result, currLeftIndex)), plus, const(1)),binOp(call(getHeight, read(Reg.result, arr), read(Reg.result, currRightIndex)), plus, const(1))))
    ))))
    Seq(main,getHeight)
  }
}
