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

import cs241e.assignments.Assembler._
import cs241e.assignments.CodeBuilders._
import cs241e.assignments.ProgramRepresentation._
import tools._
import cs241e.mips._

/** An implementation of a strategy to lay out and keep track of memory, both on the stack and on the heap. */

object MemoryManagement {

  /** To make it easier to keep track of which areas of memory are used to store which data, we
    * organize all memory in `Chunk`s. A `Chunk` is a compile-time representation of multiple
    * words of memory indexed by a sequence of `Variable`s. Each variable is assigned a fixed
    * offset from the beginning of the `Chunk`. The `Chunk` can generate the
    * code to store and load the value at the offset correxponding to each variable.
    *
    * When a `Chunk` is represented in memory at run time, word 0 of the `Chunk`
    * always holds the size of the `Chunk` in bytes. This makes it possible to deallocate
    * or copy the run-time instance of the `Chunk` in memory without knowing the specific
    * static `Chunk` that it represents. Word 1 of the `Chunk` is reserved for Assignment 11
    * (discussed in the next paragraph). To summarize, the memory layout looks as follows:
    * word 0: size in bytes (4 * (2 + n), where n = number of variables)
    * word 1: reserved for Assignment 11
    * words 2 to n+1: variables
    *
    * Starting in Assignment 11, it is also necessary to know at run time which `Variable`s
    * in the run-time `Chunk` are pointers to other `Chunk`s (have their isPointer == true).
    * To enable this, word 1 of the
    * `Chunk` always holds the number of pointer `Variable`s in the chunk. In addition,
    * all of the pointer `Variable`s are allocated first, starting from word 2
    * of the `Chunk`, followed by all of the non-pointer variables. To summarize, the
    * full memory layout looks as follows:
    * word 0: size in bytes (4 * (2 + n), where n = number of variables)
    * word 1: p = number of pointer variables
    * words 2   to 2+p-1: pointer variables
    * words 2+p to 2+n-1: non-pointer variables
    */
  case class Chunk(variables: Seq[Variable]) {
    /** For Assignment 11, the variables need to be sorted as described above, so we sort them here.
      * For earlier assignments, the order of the variables is irrelevant so sorting them here doesn't affect anything.
      */
    private val sortedVariables: Seq[Variable] = {
      val (pointers, nonPointers) = variables.partition(_.isPointer)
      pointers ++ nonPointers
    }

    /** The amount of memory needed to store the chunk in words. */
    val words: Int = 2 + variables.size
    /** The amount of memory needed to store the chunk in bytes. */
    val bytes: Int = words * 4
    /** Maps each variable to the offset of its address from the start of the `Chunk`.
      *
      * Scala hint:
      *
      * Seq('a', 'b', 'c')
      *   .zipWithIndex.map{case (letter, index) => (letter, index*2)}
      *   .toMap ==
      * Map('a' -> 0, 'b' -> 2, 'c' -> 4)
      *
      */
    private val variableToOffset: Map[Variable, Int] = {
      sortedVariables
        .zipWithIndex.map{case (letter, index) => (letter, 8+index*4)}
        .toMap
    }

    /** Generate code to load the value at the offset corresponding to `variable` into `register`.
      *
      * Assume that `baseRegister` contains the address of the beginning of the chunk.
      **/
    def load(baseRegister: Reg, register: Reg, variable: Variable): Code = {
      val target_offset = variableToOffset.get(variable).get
      LW(register, target_offset, baseRegister)
    }

    /** Generate code to store the value of `register` at the offset corresponding to `variable`.
      *
      * Assume that `baseRegister` contains the address of the beginning of the chunk.
      **/
    def store(baseRegister: Reg, variable: Variable, register: Reg): Code = {
      val target_offset = variableToOffset.get(variable).get
      SW(register, target_offset, baseRegister)
    }

    /** Generate code to initialize a `Chunk` that has just been allocated. The generated code should
      * assume that register `Reg.result` contains the address of the beginning of the `Chunk`.
      * It should write the size of the `Chunk` in bytes into word 0 of the `Chunk`.
      * It should set the values of all of the variables in the `Chunk` to 0.
      *
      * The generated code may modify the values of Reg.scratch. If you need more than one scratch
      * register, you may add new scratch registers to Reg.scala. The generated code must not modify the values
      * of any registers that are already listed in Reg.scala (except for Reg.scratch).
      *
      * Starting in Assignment 11, the generated code should also write the number of pointer variables into
      * word 1 of the `Chunk`.
      */
    def initialize: Code = {
      var ptrCnt = 0
      variables.foreach(x => {if (x.isPointer) ptrCnt += 1})
      var codes = Seq[Code](
        LIS(Reg.scratch),
        Word(encodeUnsigned(bytes)),
        SW(Reg.scratch, 0, Reg.result),
        LIS(Reg.scratch),
        Word(encodeUnsigned(ptrCnt)),
        SW(Reg.scratch, 4, Reg.result)
      )
      for (i <- variables) codes = codes.appended(store(Reg.result, i, Reg(0)))
      Block(codes)
    }


  }


  /** An abstract memory allocator that allocates memory either on the stack or on the heap. */
  abstract class MemoryAllocator {
    /** The code to initialize the memory allocator at the beginning of the program. By default,
      * no initialization is necessary.
      */
    val initCode: Code = block()

    /** Generate the code to allocate enough space to hold `chunk` and place the address of the allocated `Chunk`
      * in `Reg.result`. This code should be followed by the code generated by `chunk.initialize`.
      */
    def allocate(chunk: Chunk): Code
  }

  /** A `MemoryAllocator` that allocates `Chunk`s of memory on the stack. */
  object Stack extends MemoryAllocator {
    /** Generate the code to allocate enough space to hold `chunk` and place the address of the allocated `Chunk`
      * in `Reg.result`. This code should be followed by the code generated by `chunk.initialize`.
      *
      * The generated code may modify the values of Reg.stackPointer, Reg.result, Reg.scratch, Reg.copyChunkScratch,
      * and Reg.scratchPtrForGC. If you need more than these registers, you may add new scratch
      * registers to Reg.scala. The generated code must not modify the values of any other registers that are
      * already listed in Reg.scala.
      */
    def allocate(chunk: Chunk): Code = block(
      Comment("begin allocate on stack"),
      LIS(Reg.scratch),
      Word(encodeUnsigned(chunk.bytes)),
      SUB(Reg.stackPointer, Reg.stackPointer, Reg.scratch),
      ADD(Reg.result, Reg.stackPointer, Reg(0)),
      chunk.initialize ,
      Comment("end allocate on stack"),
    )

    /** Generate the code to deallocate the space for the `Chunk` that is at the top of the stack. To determine
      * the size of this `Chunk`, takes advantage of the convention that word 0 of each `Chunk` stores its size
      * in bytes.
      *
      * The generated code must not modify Reg.result. It may modify Reg.stackPointer and Reg.scratch.
      * If you need more than these registers, you may add new scratch registers to Reg.scala. The generated code
      * must not modify the values of any other registers that are already listed in Reg.scala.
      */
    val pop: Code = block(
      Comment("begin pop on stack"),
      LW(Reg.scratch, 0, Reg.stackPointer),
      ADD(Reg.stackPointer, Reg.scratch, Reg.stackPointer),
      Comment("end pop on stack")
    )
  }

  /** Code that copies a chunk whose address is in `fromRegister` to the address in `toRegister`.
    * `toRegister` and `fromRegister` cannot be one of the registers in `modifiedRegisters`.
    * Be careful to modify only the registers in `modifiedRegisters` in the copying code that
    * you generate.
    *
    * Also, do not use any Variables inside copyChunk, or any Code that depends on
    * them, that is, any code appearing after Block in ProgramRepresentation.scala,
    * particularly including whileLoop. This is because copyChunk will be used to
    * implement calls from one procedure to another, and it is not clear in which
    * procedure's frame such Variables are allocated.
    */
  def copyChunk(toRegister: Reg, fromRegister: Reg): Code = {
    /* The registers that may be modified by the code that will be generated. */
    val modifiedRegisters = Set(Reg.scratch, Reg.copyChunkScratch)
    require(!modifiedRegisters.contains(toRegister))
    require(!modifiedRegisters.contains(fromRegister))
    val end = new Label("endLabel")
    val begin = new Label("beginLabel")
    block(
      LW(Reg.scratch, 0, fromRegister),
      ADD(Reg.copyChunkScratch, fromRegister),
      ADD(Reg.copyChunkScratch3, toRegister),
      Define(begin),
      LIS(Reg.copyChunkScratch1),
      Word(encodeUnsigned(4)),
      beq(Reg.scratch, Reg.zero, end),
      Comment("I am in the loop of copying chunk"),
      SUB(Reg.scratch, Reg.scratch, Reg.copyChunkScratch1),
      LW(Reg.copyChunkScratch2, 0, Reg.copyChunkScratch),
      SW(Reg.copyChunkScratch2, 0, Reg.copyChunkScratch3),
      ADD(Reg.copyChunkScratch, Reg.copyChunkScratch, Reg.copyChunkScratch1),
      ADD(Reg.copyChunkScratch3, Reg.copyChunkScratch3, Reg.copyChunkScratch1),
      LIS(Reg.copyChunkScratch1),
      Use(begin),
      JR(Reg.copyChunkScratch1),
      Define(end)
    )
  }

  var heap: MemoryAllocator = SimpleHeapAllocator

  trait HeapSettings {
    /** The total number of bytes of memory. */
    val memSize = decodeUnsigned(CPU.maxAddr)
    /** The address of the beginning of the heap. */
    val heapStart = Word(encodeUnsigned(memSize / 4))

    /** The address of the middle of the heap. */
    val heapMiddle = Word(encodeUnsigned(memSize / 2))
    /** The address just after the end of the heap. */
    val heapEnd = Word(encodeUnsigned(memSize * 3 / 4))
  }

  /** A simple `MemoryAllocator` that allocates `Chunk`s of memory on the heap in a way that they
    * are never freed. Specifically, `Reg.heapPointer` is assumed to point to the next unused
    * memory address in the heap. To allocate a `Chunk` of a given size, the allocator just returns
    * the current value of `Reg.heapPointer` and increments it by the size so that it points to the
    * next unused word.
    */
  object SimpleHeapAllocator extends MemoryAllocator with HeapSettings {
    /** The code to initialize the heap allocator. */
    override val initCode: Code = block(LIS(Reg.heapPointer), heapStart)

    /** Generate the code to allocate enough space to hold `chunk` and place the address of the allocated `Chunk`
      * in `Reg.result`. This code should be followed by the code generated by `chunk.initialize`.
      *
      * Note that `allocate` is called after `VarAccess` `Code`s have been eliminated. Therefore, the `Code` that
      * it returns must not contain any `VarAccess` `Code`s or `Code`s that are defined after `VarAccess` in
      * `ProgramRepresentation.scala`.
      *
      * The generated code may modify the values of Reg.result, Reg.scratch, Reg.copyChunkScratch,
      * Reg.scratchPtrForGC, Reg.heapPointer, and Reg.fromSpaceEnd. If you need more than these
      * registers, you may add new scratch registers to Reg.scala. The generated code must not modify the values
      * of any other registers that are already listed in Reg.scala.
      */
    def allocate(chunk: Chunk): Code = block(
      Comment("begin allocate on heap"),
      LIS(Reg.scratch),
      Word(encodeUnsigned(chunk.bytes)),
      ADD(Reg.result, Reg.heapPointer),
      ADD(Reg.heapPointer, Reg.heapPointer, Reg.scratch),
      chunk.initialize,
      Comment("end allocate on heap")
    )
  }

  /* ## Assignment 11 */

  /** A `MemoryAllocator` that uses a copying garbage collector to reclaim `Chunk`s that are unreachable
    * from the current `Chunk` whose address is in `Reg.framePtr`. The heap is split into two halves (semispaces).
    * Memory is allocated from one of the semispaces. When the semispace becomes full, the garbage collector
    * is launched. The garbage collector copies all reachable `Chunk`s into the other semispace
    * and adjusts all pointer `Variable`s in all reachable `Chunk`s to point to the new copies. The other semispace
    * then becomes the current semispace from which memory is allocated until it becomes full, and then the whole
    * process is repeated.
    *
    * The provided `initCode` follows the assumption that `Reg.heapPointer` points to the next unused
    * word in the current semispace, and that `Reg.fromSpaceEnd` points to the word immediately
    * after the end of the current semispace.
    *
    * The first semispace starts at address heapStart and ends just before heapMiddle, and
    * the second semispace starts at address heapMiddle and ends just before heapEnd.
    */

  object GarbageCollector extends MemoryAllocator with HeapSettings {
    
    /** The code to initialize the heap allocator. */
    override val initCode: Code = block(LIS(Reg.heapPointer), heapStart, LIS(Reg.fromSpaceEnd), heapMiddle)

    /** Declarations of procedures of the memory allocator and of local variables used in those procedures.
      *
      * You may add more procedures and variables here. If you add procedures, be sure to add them to
      * `def procedures` below.
      */
    
    val malloc_bytes = new Variable("malloc_bytes")
    val malloc = new Procedure("malloc", Seq(malloc_bytes))
    
    val collectGarbage = new Procedure("collectGarbage", Seq())
    
    /** The sequence of all procedures required by memory allocator. This sequence must
      * contain `malloc` and `collectGarbage`, as well as any additional helper procedures that you define.
      */
    def procedures: Seq[Procedure] = Seq(malloc, collectGarbage)
    
    /** Code of the `malloc` procedure, which allocates an area of `malloc_bytes` consecutive bytes in memory
      * and places the address of the beginning of that area in `Reg.result`.
      *
      * If there is not enough space in the current semispace, `malloc` should call `collectGarbage` to try to
      * free up space. If there is still not enough space after the garbage collection pass, the behaviour of
      * `malloc` is undefined.
      *
      * The code in this procedure may modify the values of Reg.result, Reg.scratch, Reg.copyChunkScratch,
      * Reg.scratchPtrForGC, Reg.heapPointer, and Reg.fromSpaceEnd. If you need more than these
      * registers, you may add new scratch registers to Reg.scala. The code must not modify the values
      * of any other registers that are already listed in Reg.scala.
      */

    malloc.code = {
      block(
        ifStmt(binOp(ADD(Reg.result, Reg.heapPointer), plus, read(Reg.result, malloc_bytes)), gtCmp, ADD(Reg.result, Reg.fromSpaceEnd), call(collectGarbage)),
        ADD(Reg.scratchPtrForGC, Reg.heapPointer),
        read(Reg.result, malloc_bytes),
        ADD(Reg.heapPointer, Reg.heapPointer, Reg.result),
        ADD(Reg.result, Reg.scratchPtrForGC)
      )

    }
/*
    malloc.code = {
       block(
        call(collectGarbage),
         read(Reg.result, malloc_bytes),
        ADD(Reg.scratchPtrForGC, Reg.heapPointer),
        ADD(Reg.heapPointer, Reg.heapPointer, Reg.result),
        ADD(Reg.result, Reg.scratchPtrForGC)
      )}*/



    /** Generate the code to call the `malloc` procedure to allocate enough memory to hold `chunk` and place
      * the address of the allocated `Chunk` in `Reg.result`. This code should be followed by the code
      * generated by `chunk.initialize`.
      *
      * Note that `allocate` is called after `VarAccess` `Code`s have been eliminated. Therefore, the `Code` that
      * it returns must not contain any `VarAccess` `Code`s or `Code`s that are defined after `VarAccess` in
      * `ProgramRepresentation.scala`. In particular, it cannot contain the `Call` `Code`, so the code to call
      * the `malloc` procedure must be implemented directly in terms of simpler `Code`s.
      *
      * The generated code may modify the values of Reg.result, Reg.scratch, Reg.copyChunkScratch,
      * Reg.scratchPtrForGC, Reg.heapPointer, and Reg.fromSpaceEnd. If you need more than these
      * registers, you may add new scratch registers to Reg.scala. The generated code must not modify the values
      * of any other registers that are already listed in Reg.scala.
      */
    def allocate(chunk: Chunk): Code = {
      val tempStatic = new Variable("tempstatic", isPointer = true)
      val mallocbytes = new Variable("malloc")
      val tempChunk = Chunk(Seq(tempStatic, mallocbytes))
      block(
        Comment("begin allocate on heap"),
        LIS(Reg.scratchPtrForGC),
        Word(encodeUnsigned(chunk.bytes)),
        Stack.allocate(tempChunk),
        tempChunk.store(Reg.result, mallocbytes, Reg.scratchPtrForGC),
        LIS(Reg.tempTargetPC),
        Use(malloc.label),
        JALR(Reg.tempTargetPC),
        chunk.initialize
      )
    }

    /** The code of the procedure that performs a garbage collection pass over the heap. The procedure should take
      * zero parameters, and it should return in Reg.result an `Int`, the total number of bytes of memory
      * in the heap (that is, the sum of the sizes in bytes of all of the live objects remaining in the heap
      * after the garbage collection pass).
      *
      * The code in this procedure may modify the values of Reg.result, Reg.scratch, Reg.copyChunkScratch,
      * Reg.scratchPtrForGC, Reg.heapPointer, and Reg.fromSpaceEnd. If you need more than these
      * registers, you may add new scratch registers to Reg.scala. The code must not modify the values
      * of any other registers that are already listed in Reg.scala.
      *
      * You may assume that the `collectGarbage` procedure will only ever be called from a procedure whose frame
      * is stored on the stack, not on the heap. Thus, you may assume that the dynamic link in the `collectGarbage`
      * procedure points to what was the top of the stack before the `collectGarbage` procedure was called.
      */
    collectGarbage.code = {

      val free = new Variable("free", isPointer = true)
      val o = new Variable("o")
      val scan = new Variable("scan", isPointer = true)
      val scanVal = new Variable("scanVal", isPointer = true)
      val toSpaceStart = new Variable("start", isPointer = true)
      val cnt = new Variable("cnt")
      val ans = new Variable("ans")
      def copyCode(variable: Variable) = block(
        Comment("I am copying chunks"),
        ifStmt(deref(read(Reg.result, variable)), geCmp, const(0), block(
          read(Reg.result, free),
          read(Reg.scratchPtrForGC, variable),
          copyChunk(Reg.result, Reg.scratchPtrForGC),
          SW(Reg.result, 4, Reg.scratchPtrForGC),
          deref(read(Reg.result, free)),
          SUB(Reg.result, Reg.zero, Reg.result),
          SW(Reg.result, 0, Reg.scratchPtrForGC),
          assign(ans, binOp(read(Reg.result, ans), plus, deref(read(Reg.result,free)))),
          assign(free, binOp(read(Reg.result, free), plus, deref(read(Reg.result,free))))
        )),
        deref(binOp(read(Reg.result, variable), plus, const(4)))
      )
      def copy(variable: Variable):Code = {
        ifStmt(read(Reg.result, variable), ltCmp, block(LIS(Reg.result), heapStart),read(Reg.result, variable),
          ifStmt(read(Reg.result, variable), geCmp, block(LIS(Reg.result), heapEnd), read(Reg.result, variable),
            ifStmt(ADD(Reg.result,Reg.fromSpaceEnd), eqCmp, block(LIS(Reg.result), heapMiddle),
              ifStmt(read(Reg.result, variable), ltCmp, block(LIS(Reg.result), heapMiddle),copyCode(variable), read(Reg.result, variable)),
              ifStmt(read(Reg.result, variable), geCmp, block(LIS(Reg.result), heapMiddle),copyCode(variable), read(Reg.result, variable)))) )
      }
      def fowardPtrs(variable: Variable):Code={
        block(
          Comment("I am forwarding ptrs"),
          assign(cnt, block(read(Reg.result, variable), LW(Reg.result, 4, Reg.result))),
          assign(o, const(8)),
          whileLoop(read(Reg.result, cnt), gtCmp, const(0),
          block(
            assign(scanVal, deref(binOp(read(Reg.result, variable), plus, read(Reg.result, o)))),
            Comment("Here is the address"),
            assignToAddr(binOp(read(Reg.result, variable), plus, read(Reg.result, o)), copy(scanVal)),
            assign(o, binOp(read(Reg.result, o), plus, const(4))),
            assign(cnt, binOp(read(Reg.result, cnt), minus, const(1)))
          ))
        )
      }
      Scope(Seq(free, o, scan, toSpaceStart, cnt,ans,scanVal), block(
        Comment("GC begins working"),
        assign(toSpaceStart, ifStmt(ADD(Reg.result, Reg.fromSpaceEnd), eqCmp, block(LIS(Reg.result), heapMiddle), block(LIS(Reg.result), heapMiddle), block(LIS(Reg.result), heapStart))),
        assign(free, read(Reg.result, toSpaceStart)),
        assign(scan, read(Reg.result, collectGarbage.dynamicLink)),
        whileLoop(read(Reg.result, scan), ltCmp, block(LIS(Reg.result),CPU.maxAddr), block(
          Comment("I am in the first loop"),
          fowardPtrs(scan),
          assign(scan, binOp(read(Reg.result, scan), plus, deref(read(Reg.result, scan))))
        )),
        assign(scan, read(Reg.result, toSpaceStart)),
        whileLoop(read(Reg.result, scan), ltCmp, read(Reg.result, free), block(
          Comment("I am in the second loop"),
          fowardPtrs(scan),
          assign(scan, binOp(read(Reg.result, scan), plus, deref(read(Reg.result, scan))))
        )),
        ifStmt(read(Reg.result, toSpaceStart), eqCmp, block(LIS(Reg.result),heapMiddle), block(LIS(Reg.fromSpaceEnd), heapEnd), block(LIS(Reg.fromSpaceEnd), heapMiddle)),
        read(Reg.heapPointer, free),
        Comment("GC ends working"),
        read(Reg.result, ans)
      ))

    }
  }
}
