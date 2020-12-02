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

import Assembler._
import ProgramRepresentation._
import Transformations._
import cs241e.mips._

object A2 {
  /* As part of Assignment 2, before implementing the methods in this file, first implement the methods in
   * Transformations.scala in the section for Assignment 2.
   */

  /* Registers 1 and 2 hold 32-bit integers (in two's-complement notation). Place the maximum of these integers
   * in register 3, and return.
   */
  lazy val maximum: Seq[Word] = {
    val label1 = new Label("label1")
    val code = Seq[Code](
      SLT(Reg(3),Reg(1),Reg(2)),
      beq(Reg(3), Reg(0), label1),
      ADD(Reg(3),Reg(2),Reg(0)),
      BEQ(Reg(0),Reg(0),1),
      Define(label1),
      ADD(Reg(3),Reg(1),Reg(0)),
      JR(Reg(31))
    )
    eliminateLabels(code)
  }

  /* Registers 1 and 2 hold 32-bit integers (in unsigned integer notation). Place the maximum of these integers
   * in register 3, and return.
   */
  lazy val maximumUnsigned: Seq[Word] = {
    val label1 = new Label("label1")
    val code = Seq[Code](
      SLTU(Reg(3),Reg(1),Reg(2)),
      beq(Reg(3), Reg(0), label1),
      ADD(Reg(3),Reg(2),Reg(0)),
      BEQ(Reg(0),Reg(0),1),
      Define(label1),
      ADD(Reg(3),Reg(1),Reg(0)),
      JR(Reg(31))
    )
    eliminateLabels(code)
  }

}

