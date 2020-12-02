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

import scala.annotation.tailrec

/** An assembler that generates machine language words representing MIPS instructions. */

object Assembler {

  /* ## Assignment 1 */

  /* Complete the implementation of the following methods by replacing the `???`. */

  /** Given a sequence of bits, interpret it as an unsigned binary number and return the number.
    *
    * Scala hint: Consult the Scala library documentation for classes such as Seq:
    * https://www.scala-lang.org/api/current/scala/collection/immutable/Seq.html
    *
    * The purpose of this assignment is for *you* to write code that encodes/decodes numbers in binary.
    * Do not submit solutions that just call functions in the Java/Scala standard library to do the
    * conversion for you.
    **/
  def twoExp(exp: Int) : Long={
    return twoTo(exp)
  }
  def decodeUnsigned(bits: Seq[Boolean]): Long = {
    require(bits.length >= 0)
    require(bits.length <= 32)
    var sum = 0.toLong
    var index = bits.length-1
    val length = index
    for (i <- 0 to length){
      var isOne = bits(i)
      if (isOne){
        sum += twoTo(index)
      }
      index -= 1
    }
    return sum
  }

  /** Given a sequence of bits, interpret it as a signed two's-complement binary number and return the number. */
  def decodeSigned(bits: Seq[Boolean]): Long = {
    require(bits.length >= 1)
    require(bits.length <= 32)
    var sum = 0.toLong;
    var index = bits.length-1;
    val length = index;
    for (i <- 0 to length){
      var isOne = bits(i);
      if (isOne && i == 0){
        sum -= twoTo(index);
      } else if (isOne){
        sum += twoTo(index);
      }
      index -= 1;
    }
    return sum;
  }

  /** Given a non-negative number `i`, encode it as an unsigned binary number using the number of bits
    * specified by `bits`.
    *
    * Scala hint: The `bits: Int = 32` specifies `32` as the default value of bits. When calling this method, one
    * can specify the number of bits explicitly (e.g. `encodeUnsigned(42, 8)`), or leave it unspecified
    * (e.g. `encodeUnsigned(42)`), in which case the default value of 32 will be used.
    *
    * The length of the output sequence must be equal to `bits`.
    **/
  def encodeUnsigned(i: Long, bits: Int = 32): Seq[Boolean] = {
    require(bits >= 0)
    require(bits <= 32)
    require(i >= 0)
    require(i < twoTo(bits))
    var s1 = Seq[Boolean]()
    if(bits==0) return  s1
    var encodedNum = i
    var remainder = 0.toLong
    while (encodedNum > 0 && s1.length <= bits){
      remainder = encodedNum % 2
      if (remainder == 0) {
        s1 = s1.prepended(false)
      };
      else {
        s1 = s1.prepended(true)
      }
      encodedNum /= 2
    }
    while (s1.length < bits){
      s1 = s1.prepended(false)
    }
    return s1
  }
  /** Given a number `i`, encode it as a signed two's-complement binary number using the number of bits
    * specified by `bits`.
    *
    * The length of the output sequence must be equal to `bits`.
    **/
  def encodeSigned(i: Long, bits: Int = 32): Seq[Boolean] = {
    require(bits >= 1)
    require(bits <= 32)
    require(i >= -twoTo(bits-1))
    require(i < twoTo(bits-1))
    var s1 = Seq[Boolean]()
    var encodedNum = i
    var remainder = 0.toLong
    if (i >= 0){
      while (encodedNum > 0 && s1.length <= bits){
        remainder = encodedNum % 2
        if (remainder == 0) {
          s1 = s1.prepended(false)
        };
        else {
          s1 = s1.prepended(true)
        }
        encodedNum /= 2
        if (s1.length + 1 == bits){
          s1 = s1.prepended(false)
        }
      }
      while (s1.length < bits){
        s1 = s1.prepended(false)
      }
    } else {
      encodedNum = twoTo(bits-1)+i
      s1 = encodeUnsigned(encodedNum,bits-1).prepended(true)
    }

    return s1
  }


  /* Before continuing Assignment 1, go to `A1.scala` and complete the methods there. Then return here and implement
   * the following.
   */

  

  /* Each of the following methods should encode the corresponding MIPS machine language instruction as a 32-bit `Word`.
   *
   * Hint: One way to create a word is from a sequence of 32 Booleans.
   * One way to create a sequence of Booleans is using Bits.
   *
   * For example:
   * `val fourBits = Seq(true, false, true, false)`
   * `val moreBits = Bits("0101")`
   * `val eightBits = fourBits ++ moreBits`
   * `val word = Word(eightBits ++ eightBits ++ eightBits ++ eightBits)`
   */

  /* Hint: You may implement additional helper methods if you wish to factor out common code. */


  def RInsTemp(d: Reg, s: Reg, t : Reg = Reg.zero): Seq[Boolean] = {
    var opcode = Bits("000000")
    var dBits = encodeUnsigned(d.number,5)
    var sBits = encodeUnsigned(s.number, 5)
    var tBits = encodeUnsigned(t.number, 5)
    var shamt = encodeUnsigned(0, bits = 5)
    return opcode ++ sBits ++ tBits ++ dBits ++ shamt
  }

  def RInsTemp2(s: Reg, t:Reg): Seq[Boolean] ={
    var opcode = Bits("000000")
    var sBits = encodeUnsigned(s.number, 5)
    var tBits = encodeUnsigned(t.number, 5)
    var shamt = encodeUnsigned(0, bits = 10)
    return opcode ++ sBits ++ tBits ++ shamt
  }

  def ADD(d: Reg, s: Reg, t: Reg = Reg.zero): Word = {
    var funct = encodeUnsigned(32, bits = 6)
    return Word(RInsTemp(d,s,t) ++ funct)
  }
  def SUB(d: Reg, s: Reg, t: Reg): Word = {
    var funct = encodeUnsigned(34, bits = 6)
    return Word(RInsTemp(d,s,t) ++ funct)
  }
  def MULT(s: Reg, t: Reg): Word = {
    var funct = encodeUnsigned(24,6)
    return Word(RInsTemp2(s,t) ++ funct)
  }
  def MULTU(s: Reg, t: Reg): Word = {
    var funct = encodeUnsigned(25,6)
    return Word(RInsTemp2(s,t) ++ funct)
  }
  def DIV(s: Reg, t: Reg): Word = {
    var funct = encodeUnsigned(26, 6)
    return Word(RInsTemp2(s,t) ++ funct)
  }
  def DIVU(s: Reg, t: Reg): Word = {
    var funct = encodeUnsigned(27, 6)
    return Word(RInsTemp2(s,t) ++ funct)
  }
  def MFHI(d: Reg): Word = {
    var opcode = Bits("000000")
    var HIcode = encodeUnsigned(0, bits = 10)
    var dcode = encodeUnsigned(d.number, 5)
    var shamt = encodeUnsigned(0, bits = 5)
    var funct = encodeUnsigned(16,6)
    return Word(opcode ++ HIcode ++ dcode ++ shamt ++ funct)
  }
  def MFLO(d: Reg): Word = {
    var opcode = Bits("000000")
    var HIcode = encodeUnsigned(0, bits = 10)
    var dcode = encodeUnsigned(d.number, 5)
    var shamt = encodeUnsigned(0, bits = 5)
    var funct = encodeUnsigned(18,6)
    return Word(opcode ++ HIcode ++ dcode ++ shamt ++ funct)
  }
  def LIS(d: Reg): Word = {
    var opcode = Bits("000000")
    var shamt1 = encodeUnsigned(0,10)
    var dcode = encodeUnsigned(d.number,5)
    var shamt2 = encodeUnsigned(0,5)
    var funct = Bits("010100")
    return Word(opcode ++ shamt1 ++dcode ++shamt2 ++funct)
  }
  def LW(t: Reg, i: Int, s: Reg): Word = {
    var opcode = Bits("100011")
    var tcode = encodeUnsigned(t.number,5)
    var scode = encodeUnsigned(s.number,5)
    var immediate = encodeSigned(i,16)
    return Word(opcode ++ scode ++ tcode ++ immediate)
  }
  def SW(t: Reg, i: Int, s: Reg): Word = {
    var opcode = Bits("101011")
    var tcode = encodeUnsigned(t.number,5)
    var scode = encodeUnsigned(s.number,5)
    var immediate = encodeSigned(i,16)
    return Word(opcode ++ scode ++ tcode ++ immediate)
  }
  def SLT(d: Reg, s: Reg, t: Reg): Word = {
    var funct = Bits("101010")
    return Word(RInsTemp(d, s, t) ++ funct)
  }
  def SLTU(d: Reg, s: Reg, t: Reg): Word = {
    var funct = Bits("101011")
    return Word(RInsTemp(d, s, t) ++ funct)
  }
  def BEQ(s: Reg, t: Reg, i: Int): Word = {
    var BEQ = Bits("000100")
    var tcode = encodeUnsigned(t.number,5)
    var scode = encodeUnsigned(s.number,5)
    var immediate = encodeSigned(i,16)
    return Word(BEQ ++ scode ++ tcode ++ immediate)
  }
  def BNE(s: Reg, t: Reg, i: Int): Word = {
    var BEQ = Bits("000101")
    var tcode = encodeUnsigned(t.number,5)
    var scode = encodeUnsigned(s.number,5)
    var immediate = encodeSigned(i,16)
    return Word(BEQ ++ scode ++ tcode ++ immediate)
  }
  def JR(s: Reg): Word = {
    var opcode = Bits("000000")
    var scode = encodeUnsigned(s.number,5)
    var shamt = encodeUnsigned(0,15)
    var funct = Bits("001000")
    return Word(opcode ++ scode ++ shamt ++funct)
  }
  def JALR(s: Reg): Word = {
    var opcode = Bits("000000")
    var scode = encodeUnsigned(s.number,5)
    var shamt1 = encodeUnsigned(0,5)
    var returncode = encodeUnsigned(0,5)
    var shamt2 = encodeUnsigned(0,5)
    var funct = Bits("001001")
    return Word(opcode ++ scode ++shamt1 ++ returncode ++shamt2 ++funct)
  }

}
