import java.lang.annotation.Annotation

import cs241e.assignments.Assembler._
import cs241e.assignments.CodeBuilders._
import cs241e.assignments.MemoryManagement._
import cs241e.assignments.ProgramRepresentation._
import cs241e.assignments.Transformations.compilerA6
import cs241e.assignments.{A4, CodeGenerator, Lacs, MemoryManagement, Reg, Typer}
import cs241e.mips.{State, Word}
import cs241e.assignments.tools._
import org.scalatest.FunSuite

class A11Tests extends FunSuite {
  test("test2") {
    def compileAndRun(prog: String, a: Int = 0, b: Int = 0, debug: Boolean = false): Int = {
      val machineCode = Lacs.compileWithGarbageCollector(prog)
      val finalState = A4.loadAndRun(machineCode, Word(encodeSigned(a)), Word(encodeSigned(b)), debug)
      decodeSigned(finalState.reg(3)).toInt
    }
    MemoryManagement.GarbageCollector.malloc.code = {
      block(
        call(GarbageCollector.collectGarbage),
        ADD(Reg.scratchPtrForGC, Reg.heapPointer),
        read(Reg.result, GarbageCollector.malloc_bytes),
        ADD(Reg.heapPointer, Reg.heapPointer, Reg.result),
        ADD(Reg.result, Reg.scratchPtrForGC)
      )

    }
    //    pprint.pprintln(Lacs.scanAndParseAndTypeAndGenerate(prog).head.code)
    // GC only runs if:
    // - program uses closures and
    // - program allocate so many closures that from-space fills up
    // So: for testing, modify malloc.code to ALWAYS call collectGarbage,
    // not only when from-space is full.
    val prog3 =
      """
    def main(a: Int, b: Int): Int = {
      var proc: ()=>Int;
       proc = increaseBy();
      (increaseBy())()
    }
           def increaseBy(): ()=>Int = {
      def procedure(): Int = { 10 }
      procedure
    }
      """
    println(compileAndRun(prog3, 6, 5,debug = true))
  }

  test("heaps") {
    MemoryManagement.heap = GarbageCollector
    MemoryManagement.GarbageCollector.malloc.code = {
      block(
        call(GarbageCollector.collectGarbage),
        ADD(Reg.scratchPtrForGC, Reg.heapPointer),
        read(Reg.result, GarbageCollector.malloc_bytes),
        ADD(Reg.heapPointer, Reg.heapPointer, Reg.result),
        ADD(Reg.result, Reg.scratchPtrForGC)
      )
    }
    val f = new Variable("f", isPointer = true)
    val chunk = Chunk(Seq(f))
    //val proc = new Procedure("proc", Seq())
    //proc.code = block()

    val a = new Variable("a")
    val b = new Variable("b")
    val p = new Variable("p", isPointer = true)
    val q = new Variable("q", isPointer = true)
    val main = new Procedure("main", Seq(a, b))

    def newChunk = MemoryManagement.heap.allocate(chunk)

    def v(variable: Variable) = read(Reg.result, variable)

    def writeField(base: Code, field: Variable, value: Code): Code =
      binOp(base, chunk.store(Reg.scratch, field, Reg.result), value)

    main.code = Scope(Seq(p, q), block(
      assign(p, newChunk),
      assign(q, newChunk),
      writeField(v(q), f, v(p)),
            writeField(v(p), f, v(q)), // create a cycle
      assign(p, v(q)),
      call(GarbageCollector.collectGarbage),
      call(GarbageCollector.collectGarbage)
      //ADD(Reg.zero, Reg.zero)
    ))

    val machineCode = compilerA6(main +: GarbageCollector.procedures)
    val finalState = A4.loadAndRun(machineCode)

    def dumpMem(state: State, address: Word, words: Int = 6): Unit = {
      if(words > 0) {
        println(spaces(address) + ": " + spaces(state.mem(address)))
        dumpMem(state, Word(encodeUnsigned(decodeUnsigned(address) + 4)), words-1)
      }
    }

    def spaces(w: Word): String = w.toString.sliding(8,8).mkString(" ")

    println("Semispace 1")
    dumpMem(finalState, GarbageCollector.heapStart)
    println("Semispace 2")
    dumpMem(finalState, GarbageCollector.heapMiddle)
    println("reachable: "+decodeUnsigned(finalState.reg(3)))
    println("heapPtr: "+spaces(finalState.reg(Reg.heapPointer.number)))
  }
  test("heaps2") {
    MemoryManagement.heap = GarbageCollector
    MemoryManagement.GarbageCollector.malloc.code = {
      block(
        call(GarbageCollector.collectGarbage),
        ADD(Reg.scratchPtrForGC, Reg.heapPointer),
        read(Reg.result, GarbageCollector.malloc_bytes),
        ADD(Reg.heapPointer, Reg.heapPointer, Reg.result),
        ADD(Reg.result, Reg.scratchPtrForGC)
      )
    }
    val f = new Variable("f", isPointer = true)
    val chunk = Chunk(Seq(f))
    //val proc = new Procedure("proc", Seq())
    //proc.code = block()

    val a = new Variable("a")
    val b = new Variable("b")
    val p = new Variable("p", isPointer = true)
    val q = new Variable("q", isPointer = true)
    val main = new Procedure("main", Seq(a, b))
    val closure = new Procedure("closure", Seq())
    closure.code = const(10)

    def v(variable: Variable) = read(Reg.result, variable)

    main.code = Scope(Seq(q),block(
      assign(q, Closure(closure)),
      CallClosure(v(q), Seq(), Seq()),
    ))

    val machineCode = compilerA6(Seq(main, closure)++ GarbageCollector.procedures)
    val finalState = A4.loadAndRun(machineCode, debug = true)

    def dumpMem(state: State, address: Word, words: Int = 6): Unit = {
      if(words > 0) {
        println(spaces(address) + ": " + spaces(state.mem(address)))
        dumpMem(state, Word(encodeUnsigned(decodeUnsigned(address) + 4)), words-1)
      }
    }

    def spaces(w: Word): String = w.toString.sliding(8,8).mkString(" ")

    println("Semispace 1")
    dumpMem(finalState, GarbageCollector.heapStart)
    println("Semispace 2")
    dumpMem(finalState, GarbageCollector.heapMiddle)
    println("reachable: "+decodeUnsigned(finalState.reg(3)))
    println("heapPtr: "+spaces(finalState.reg(Reg.heapPointer.number)))
  }
}