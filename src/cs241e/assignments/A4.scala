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

import cs241e.mips._
import Assembler._
import Transformations._
import ProgramRepresentation.{read, _}
import CodeBuilders._
import A1._
import tools._

object A4 {
  /** This is an enhanced version of the loadAndRun method from Assignment 1.
    *
    * It loads the give machine language code into memory, writes the specified values to registers 1 and 2,
    * and then runs the CPU on the code that was loaded. The debugger can be invoked by passing the argument
    * debug = true.
    */
  def loadAndRun(code: MachineCode, register1: Word = Word.zero, register2: Word = Word.zero, debug: Boolean = false): State = {
    val initialState =
      setMem(code.words)
        .setReg(1, register1)
        .setReg(2, register2)
    if(debug) Debugger.debug(initialState, code.debugTable)
    else CPU.run(initialState)
  }

  /** A utility method that takes two sequences of words representing `code` and an `array`,
    * loads both into memory, and sets register 1 to the address of the beginning
    * of the array and register 2 to its length in words.
    */
  def loadCodeAndArray(code: Seq[Word], array: Seq[Word]): State = {
    val arrayAddress: Word = Word(encodeUnsigned(code.size * 4))
    val arrayLength: Word = Word(encodeUnsigned(array.size))
    val loadedCode: State = setMem(code)
    val loadedArray: State = setMem(array, loadedCode, arrayAddress)
    loadedArray.setReg(1, arrayAddress).setReg(2, arrayLength)
  }

  /** A utility method that loads code and an array into memory and runs the code.
    * The debugger can be invoked by passing the argument debug = true.
    */
  def loadAndRunArray(code: MachineCode, array: Seq[Word], debug: Boolean = false): State = {
    val state = loadCodeAndArray(code.words, array)
    if(debug) Debugger.debug(state, code.debugTable)
    else CPU.run(state)
  }

  /** Register 1 holds the address of the beginning of an array of 32-bit integers.
    * Register 2 holds the number of elements in the array.
    * If the array is empty place the value -1 in register 3.
    * Otherwise copy the last element of the array into register 3.
    */
  lazy val lastElement: MachineCode = {
    val code: Code = ifStmt(ADD(Reg.result, Reg(2), Reg(0)), eqCmp, ADD(Reg.result, Reg.zero, Reg.zero),
      addI(Reg.result, Reg.zero, -1),
      block(
        binOp(const(4), times, binOp(ADD(Reg.result, Reg(2), Reg(0)),minus, const(1))),
        ADD(Reg.result, Reg(1), Reg.result),
        LW(Reg.result, 0, Reg.result)
      ))
    compilerA4(code)
  }

  /** Register 1 holds the address of the beginning of an array of 32-bit two's-complement integers.
    * Register 2 holds the number of elements in the array.
    * Determine the maximum of all the elements of the array, write it into register 3.
    * Assume the array is not empty.
    */
  lazy val arrayMaximum: MachineCode = {
    val currentMax = new Variable("max")
    val currentIndex = new Variable("index")
    val code: Code = Scope(Seq(currentMax, currentIndex),block(
      LW(Reg.result,0, Reg(1)),
      write(currentMax, Reg.result),
      whileLoop(read(Reg.result, currentIndex), ltCmp, ADD(Reg.result, Reg(2), Reg(0)),
      block(
        binOp(read(Reg.result, currentIndex), times, const(4)),
        ADD(Reg.scratchExtend1, Reg.result, Reg(1)),
        ifStmt(read(Reg.result, currentMax), ltCmp, LW(Reg.result, 0, Reg.scratchExtend1),
        block(
          LW(Reg.result, 0, Reg.scratchExtend1),
          write(currentMax, Reg.result),
          read(Reg.result, currentIndex),
          addI(Reg.result, Reg.result, 1),
          write(currentIndex, Reg.result)
        ),block(
            read(Reg.result, currentIndex),
            addI(Reg.result, Reg.result, 1),
            write(currentIndex, Reg.result)
          ))
      )),read(Reg.result, currentMax)
    )
    )
    compilerA4(code)
  }

  /** Register 1 holds the address of the beginning of an array of 32-bit integers, each representing a character.
    * The integer zero represents a space, and each integer i (1 <= i <= 26) represents the i'th letter of the
    * uppercase alphabet.
    * Register 2 holds the number of elements in the array (can be empty).
    * Your program should output the uppercase characters represented by the integers in the array.
    * The MIPS system allows you to output one character at a time, by storing its ASCII value into the
    * special memory location ffff000c (hex).
    *
    * Hint: use Google to find an ASCII code table.
    */
  
  lazy val outputLetters: MachineCode = {
    val sepcialLocation = encodeUnsigned(4294901772L)
    val currentIndex = new Variable("currentIndex")
    val currentVal = new Variable("currentVal")
    val code: Code = Scope(Seq(currentIndex, currentVal),block(
      LIS(Reg.scratchExtend1),
      Word(sepcialLocation),
      whileLoop(read(Reg.result, currentIndex), ltCmp, ADD(Reg.result, Reg(2), Reg.zero),
      block(
        binOp(binOp(read(Reg.result, currentIndex), times, const(4)), plus, ADD(Reg.result, Reg(1), Reg.zero)),
        LW(Reg.result, 0, Reg.result),
        write(currentVal, Reg.result),
        ifStmt(read(Reg.result, currentVal), eqCmp, const(0),
          block(
            LIS(Reg.result),
            Word(encodeSigned(32)),
            SW(Reg.result, 0,Reg.scratchExtend1)
        ),
          block(
            read(Reg.result, currentVal),
            addI(Reg.result, Reg.result, 64),
            SW(Reg.result, 0,Reg.scratchExtend1)
        )),
        read(Reg.result, currentIndex),
        addI(Reg.result, Reg.result, 1),
        write(currentIndex, Reg.result)
      ))
    ))
    compilerA4(code)
  }

  /** Register 1 holds a 32-bit integer (in two's-complement notation).
    * Your program should format this integer in base 10, print it, then print a newline character.
    */

  lazy val printIntegerCode: Code = {
    val targetNum = new Variable("num")
    val base = new Variable("base")
    val currentVal = new Variable("val")
    val Ten = new Variable("ten")
    val temp = new Variable("temp")
    val sepcialLocation = encodeUnsigned(4294901772L)
    Scope(Seq(targetNum, base,currentVal,Ten, temp), block(
      assign(Ten, const(10)),
      LIS(Reg.scratchExtend1),
      Word(sepcialLocation),
      write(targetNum, Reg(1)),
      baseTen(targetNum),
      write(base,Reg.result),
      ifStmt(read(Reg.result, targetNum), ltCmp, const(0),block(
        LIS(Reg.result),
        Word(encodeSigned(45)),
        SW(Reg.result, 0, Reg.scratchExtend1))),
      whileLoop(read(Reg.result,base),geCmp, const(0),block(
        binOp(read(Reg.result,targetNum), divide, EXP(Ten,base)),
        write(currentVal,Reg.result),
        ifStmt(read(Reg.result, currentVal), ltCmp, const(0),block(
          binOp(read(Reg.result, currentVal), times, const(-1)),
          write(currentVal,Reg.result)
        )),
        read(Reg.result, currentVal),
        addI(Reg.result,Reg.result,48),
        SW(Reg.result, 0, Reg.scratchExtend1),
        ifStmt(read(Reg.result, targetNum), ltCmp, const(0),
          ifStmt(read(Reg.result, targetNum), leCmp, EXP(Ten, base),block(
            binOp(read(Reg.result, targetNum), plus, binOp(read(Reg.result,currentVal), times, EXP(Ten,base))),
            write(targetNum, Reg.result)
          )),
          ifStmt(read(Reg.result, targetNum), geCmp, EXP(Ten, base),block(
            binOp(read(Reg.result, targetNum), minus, binOp(read(Reg.result,currentVal), times, EXP(Ten,base))),
            write(targetNum, Reg.result)
          ))),
        read(Reg.result, base),
        subI(Reg.result,Reg.result,1),
        write(base,Reg.result),
      )),
      LIS(Reg.result),
      Word(encodeSigned(10)),
      SW(Reg.result, 0, Reg.scratchExtend1)
      )
    )
  }
  lazy val printInteger: MachineCode = compilerA4(printIntegerCode)
}
