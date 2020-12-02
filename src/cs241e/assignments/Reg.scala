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

/** Here, we give symbolic names to registers to keep track of what we will use each register for. Feel free
  * to add your own register names here for your own uses of registers.
  */
object Reg {
  /** Holds the old program counter value after a JALR instruction has executed. */
  val link = Reg(31)
  /** Address of the top item in the stack. */
  val stackPointer = Reg(30)
  /** Address of the frame of the currently executing procedure. */
  val framePointer = Reg(29)
  /** Address of the next free word of memory on the heap. */
  val heapPointer = Reg(28)
  /** Address just after the end of the current semi-space of the heap. */
  val fromSpaceEnd = Reg(27)

  /** The parameters given as input to the program. */
  val input1 = Reg(1)
  val input2 = Reg(2)
  /** Contains the result of the most recently evaluated expression. */
  val result = Reg(3)
  /** Scratch register to be used for storing temporary results during evaluation of an expression. */
  val scratch = Reg(4)
  val scratchExtend1 = Reg(10)
  val scratchExtend2 = Reg(11)
  val scratchExtend3 = Reg(12)
  val scratchExtend4 = Reg(13)
  /** In a procedure prologue, temporarily holds the address of the Chunk holding the arguments to the
    * procedure while the frame for the procedure is being created.
    */
  val savedParamPtr = Reg(5)
  /** An extra scratch register available to be used in the `copyChunk` method. */
  val copyChunkScratch = Reg(7)
  val copyChunkScratch1 = Reg(14)
  val copyChunkScratch2 = Reg(15)
  val copyChunkScratch3 = Reg(16)
  val tempTargetPC = Reg(17)
  val tempAddEntry = Reg(18)
  val temp1 = Reg(19)
  val temp2 = Reg(20)
  val temp3 = Reg(21)
  /** Used to hold the address of the procedure to be called in the implementation of a call to a procedure. */
  val targetPC = Reg(8)
  /** An extra scratch register available to be used by the garbage collector. */
  val scratchPtrForGC = Reg(9)

  /** The special register that always has the value zero. */
  val zero = Reg(0)
}

case class Reg(number: Int) {
  require(number >= 0)
  require(number <= 31)
}
