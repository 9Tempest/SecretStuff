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
import cs241e.Utils._
import State._
import Assembler._

import scala.annotation.tailrec

/** This is a tracer/debugger for the MIPS machine that is provided for your convenience in debugging your
  * MIPS programs. Feel free to modify and improve the debugger for your needs.
  */

object Debugger {
  /** Attempt to disassemble a word into a MIPS instruction, and return in the form of a `String`. */
  def disassemble(instruction: Word): String = {
    def invalidInstruction = instruction.toString

    val List(op, sBits, tBits, iBits) = instruction.splitAt(List(6, 5, 5))
    val s = decodeUnsigned(sBits)
    val t = decodeUnsigned(tBits)
    val i = decodeSigned(iBits)
    op match {
      case Bits("000000") =>
        val List(dBits, zeros, function) = iBits.splitAt(List(5, 5))
        val d = decodeUnsigned(dBits)
        if (zeros != Bits("00000")) invalidInstruction
        else function match {
          case Bits("100000") => s"add $$$d, $$$s, $$$t"
          case Bits("100010") => s"sub $$$d, $$$s, $$$t"
          case Bits("011000") if d == 0 => s"mult $$$s, $$$t"
          case Bits("011001") if d == 0 => s"multu $$$s, $$$t"
          case Bits("011010") if d == 0 => s"div $$$s, $$$t"
          case Bits("011011") if d == 0 => s"divu $$$s, $$$t"
          case Bits("010000") if s == 0 && t == 0 => s"mfhi $$$d"
          case Bits("010010") if s == 0 && t == 0 => s"mflo $$$d"
          case Bits("010100") if s == 0 && t == 0 => s"lis $$$d"
          case Bits("101010") => s"slt $$$d, $$$s, $$$t"
          case Bits("101011") => s"sltu $$$d, $$$s, $$$t"
          case Bits("001000") if t == 0 && d == 0 => s"jr $$$s"
          case Bits("001001") if t == 0 && d == 0 => s"jalr $$$s"
          case _ => invalidInstruction
        }

      case Bits("100011") => s"lw $$$t, $i($$$s)"
      case Bits("101011") => s"sw $$$t, $i($$$s)"
      case Bits("000100") => s"beq $$$s, $$$t, $i"
      case Bits("000101") => s"bne $$$s, $$$t, $i"
      case _ => invalidInstruction
    }
  }

  /** Associates optional strings (such as labels or comments) with each address.
    * The strings are printed during debugging when execution reaches the address.
    */
  type DebugTable = Map[Word, String]

  /** Runs the CPU starting from `state`, printing information about the CPU state at
    * each step. Whenever execution reaches an address that appears in the `debugTable`,
    * the corresponding string (comment/label) is printed as well.
    *
    * Feel free to modify this method to print whatever state you need to debug.
    */
  @tailrec def debug(state: State, debugTable: DebugTable = Map.empty): State = {
    if(state.reg(PC) == CPU.terminationPC) state
    else {
      if(debugTable.isDefinedAt(state.reg(PC))) print(debugTable(state.reg(PC)))

      var disasmed = disassemble(state.mem(state.reg(PC)))

      // For a lis instruction, also print the following word.
      if(disasmed.startsWith("lis ")) {
        val constant = state.mem(CPU.incrementAddress(state.reg(PC)))
        disasmed += "; " + constant
      }

      // Redefine registersToPrint to inspect other registers.
      val registersToPrint = List(3,4 ,9,28,30)
      def printReg(reg: Int) = s"$reg: ${state.reg(reg)}   "

      print(registersToPrint.map(printReg).mkString + "   ")
      println(s"${state.reg(PC)}: ${disasmed}")

      debug(CPU.step(state), debugTable)
    }
  }
}
