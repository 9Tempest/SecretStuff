import cs241e.assignments.Assembler._
import cs241e.assignments.CodeBuilders._
import cs241e.assignments.ProgramRepresentation._
import cs241e.assignments.Transformations._
import cs241e.assignments.tools._
import cs241e.assignments.{A4, Reg,A5}
import cs241e.mips.Word
import org.scalatest.FunSuite

class A5Tests extends FunSuite {
  test("scala") {
    //    def proc(i: Int): Int = {
    //      i
    //    }
    def fact(i: Int): Int = {
      val iMinusOne = i - 1
      if(i<=0) 1 else i * fact(iMinusOne)
    }
    println(fact(10))
  }

  def v(variable: Variable) = read(Reg.result, variable)
  def const(c: Int) = block(
    LIS(Reg.result),
    Word(encodeSigned(c))
  )

  // be sure to test both reading and writing both variables and parameters

  test("lacs") {
    val i = new Variable("i")
    val iMinusOne = new Variable("iMinusOne")
    val fact = new Procedure("fact", Seq(i))
    fact.code = Scope(Seq(iMinusOne), block(
      assign(iMinusOne, binOp(v(i), minus, const(1))),
      ifStmt(v(i), leCmp, const(0), const(1),
        binOp(v(i), times, call(fact, v(iMinusOne))))
    ))

    val a = new Variable("a")
    val b = new Variable("b")
    val main = new Procedure("main", Seq(a, b))
    main.code = call(fact, const(10))

    val machineCode = compilerA5(Seq(main, fact))
    val endState = A4.loadAndRun(machineCode, debug = false)
    println(decodeSigned(endState.reg(3)))
  }

  test("simple"){
    val i = new Variable("i")
    val j = new Variable("j")
    val addOne = new Procedure("addOne", Seq(i))
    addOne.code = block(
      read(Reg.result, i)
    )
    val a = new Variable("a")
    val b = new Variable("b")
    val main = new Procedure("main", Seq(a, b))
    main.code = Scope(Seq(j),block(
      assign(j, binOp(read(Reg.result, a), plus, read(Reg.result, b))),
      call(addOne, read(Reg.result, j))
    ))
    val machineCode = compilerA5(Seq(main, addOne))
    val endState = A4.loadAndRun(machineCode, Word(encodeSigned(-2)), Word(encodeSigned(-4)), debug = true)
    println(decodeSigned(endState.reg(3)))
  }

  test("more simple"){
    val a = new Variable("a")
    val b = new Variable("b")
    val main = new Procedure("main", Seq(a, b))
    main.code = block(
      Comment("main started"),
      read(Reg.result, a)
    )
    val machineCode = compilerA5(Seq(main))
    println(machineCode.debugTable)
    val endState = A4.loadAndRun(machineCode, debug = true)
    println(decodeSigned(endState.reg(3)))
  }
  test("a5"){
    val a = new Variable("a")
    val b = new Variable("b")
    val main = new Procedure("main", Seq(a, b))
    main.code = block(
      call(A5.printArray, read(Reg.result, a), read(Reg.result, b))
    )
    val machineCode = compilerA5(Seq(A5.printArray, A5.printProcedure))
    val endState = A4.loadAndRunArray(machineCode, generatrArray(Seq(10,3,2,1)))
  }

  test("tree"){
    val machineCode = compilerA5(A5.treeHeight)
    val endState = A4.loadAndRunArray(machineCode, generatrArray(Seq(7,3,6,22,-1,-1,-8,9,12,-36,-1,-1,999,-1,15, 10, -1,-1)), debug = true)
    println(decodeSigned(endState.reg(3)))
  }
  test("testIf"){
    val temp1 = new Variable("temp1")
    val temp2 = new Variable("temp2")
    val temp3 = new Variable("temp3")
    val a = new Variable("a")
    val b = new Variable("b")
    val main = new Procedure("main", Seq(a,b))
    main.code = Scope(Seq(temp1,temp2,temp3), block(
      assign(temp1, const(3)),
      assign(temp2, const(4)),
      assign(temp3, const(5)),
      ifStmt(read(Reg.result, temp1), ltCmp, read(Reg.result, temp2), ifStmt(read(Reg.result, temp1), ltCmp, read(Reg.result, temp3), const(10), const(11)),
        ifStmt(read(Reg.result, temp2), ltCmp, read(Reg.result, temp3), const(12), const(13)))
    ))
    val machineCode = compilerA5(Seq(main))
    val endState = A4.loadAndRun(machineCode)
    println(decodeSigned(endState.reg(3)))
  }
}
