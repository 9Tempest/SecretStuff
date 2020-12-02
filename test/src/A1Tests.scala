import cs241e.assignments.Assembler._
import cs241e.assignments.CodeBuilders._
import cs241e.assignments.MemoryManagement.{Chunk, Stack}
import cs241e.assignments.ProgramRepresentation._
import cs241e.assignments.Transformations._
import cs241e.assignments.{A1, A4, Debugger, Reg}
import cs241e.mips.{CPU, Word}
import org.scalatest.FunSuite
import cs241e.assignments.tools._

class A4Tests extends FunSuite {
  test("scala") {
    def proc(): Int = {
      var v = 5
      if (1 + 1 == 2) v = 2 * (3 + v) else v = 42
      v
    }
    println(proc())
  }

  // be sure to test things like this:
  //      if ((if(1==1) 2 else 3)  == 5) 6 else 7

  test("lacs") {
    val v = new Variable("v")

    val f = new Variable("f")
    val label1 = new Label("end")
    def const(c: Int) = block(
      LIS(Reg.result),
      Word(encodeSigned(c))
    )
    val code = Scope(Seq(v), block(
      assign(v, const(5)),
      ifStmt(binOp(const(1), plus, const(1)), ltCmp, const(2),
        assign(v, binOp(const(2), times, binOp(const(3), plus, read(Reg.result, v)))),
        assign(v, const(42))
      ),
      read(Reg.result, v)
    ))
    val code5 = Scope(Seq(v), block(
      assign(v, const(5)),
      ifStmt(read(Reg.result, v), leCmp, const(2),
        ifStmt(const(4), ltCmp, const(4), block(LIS(Reg(10)), Word(encodeSigned(100))), block(LIS(Reg(10)), Word(encodeSigned(50))))
        , assign(v, const(42))),
      read(Reg.result, v)
    ))


    val code3 = Scope(Seq(v), block(
      const(6),
      write(v,Reg.result),
      const(5),
      read(Reg.scratch, v),
      geCmp(label1),
      LIS(Reg(10)),
      Word(encodeSigned(100)),
      Define(label1)
    ))
    val code4 = ifStmt(const(6), eqCmp, const(6), block(LIS(Reg(10)), Word(encodeSigned(100))), block(LIS(Reg(10)), Word(encodeSigned(50))))
    //    pprint.pprintln(code)
    //pprint.pprintln(eliminateIfStmts(code))
    val machineCode = compilerA4(code5)
    val endState = A4.loadAndRun(machineCode,debug = true)
    println(decodeSigned(endState.reg(3)))

  }

  test("A4"){
   // val endState = A4.loadAndRunArray(A4.outputLetters, generatrArray(Seq(3)))
    var v1 = new Variable("v1")
    var v2 = new Variable("v2")
    val code = Scope(Seq(v1, v2), block(
      assign(v1, const(10)),
      assign(v2, const(10)),
      EXP(v1,v2)
    ))
    val machineCode = compilerA4(code)
    println(decodeSigned(A4.loadAndRun(machineCode).reg(3)))
  }

  test("final"){
    val code = block(
      LIS(Reg(1)),
      Word(encodeSigned(-1)),
      A4.printIntegerCode
    )
    val machineCode = compilerA4(code)
    println(1)
    A4.loadAndRun(machineCode)
  }

  test("whileloop"){
    val mutate = new Variable("mutate")
    val code = Scope(Seq(mutate), block(
      assign(mutate,const(0)),
      whileLoop(read(Reg.result, mutate), leCmp, const(6), block(
        read(Reg.result, mutate),
        ADD(Reg(11), Reg.result, Reg(11)),
        addI(Reg.result, Reg.result,1),
        write(mutate, Reg.result)
      )),
      read(Reg.result, mutate)
    ))
    val machineCode = compilerA4(code)
    val finalState = A4.loadAndRun(machineCode)
    println(decodeSigned(finalState.reg(3)))
    println(decodeSigned(finalState.reg(11)))
  }

}
