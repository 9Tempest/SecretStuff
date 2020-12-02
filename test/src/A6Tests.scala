import com.sun.org.apache.xml.internal.dtm.ref.IncrementalSAXSource
import cs241e.assignments.Assembler._
import cs241e.assignments.CodeBuilders._
import cs241e.assignments.ProgramRepresentation._
import cs241e.assignments.Transformations._
import cs241e.assignments.{A4, Reg}
import cs241e.mips.Word
import org.scalatest.FunSuite

class A6Tests extends FunSuite {
  test("scala") {
    def f() ={
      val w = 5
      def g() = {
        w
      }
      def h() = {
        def k() ={
          w + g()
        }
        w + g() + k()
      }
      w + h()
    }
    println(f())
  }

  def v(variable: Variable) = read(Reg.result, variable)
  def const(c: Int) = block(
    LIS(Reg.result),
    Word(encodeSigned(c))
  )

  // be sure to test both reading and writing both variables and parameters
  test("simplenest"){
    val w = new Variable("w")
    val f = new Procedure("f", Seq())
    val h = new Procedure("h", Seq(), outer = Some(f))
    val g = new Procedure("g", Seq(), outer = Some(h))
    val p = new Procedure("p", Seq(), outer = Some(f))
    f.code = Scope(Seq(w), block(
      assign(w, const(6)),
      call(h)
    ))
    h.code = binOp(call(g), plus, call(p))
    g.code = binOp(call(p),plus, v(w))
    p.code = v(w)
    val a = new Variable("a")
    val b = new Variable("b")
    val main = new Procedure("main", Seq(a, b))
    main.code = call(f)
    val machineCode = compilerA6(Seq(main, f, h, g, p))
    val endState = A4.loadAndRun(machineCode, debug = true)
    println(decodeSigned(endState.reg(3)))
  }

  test("pointer simple"){
    val w = new Variable("w")
    val temp1 = new Variable("temp1", isPointer = true)
    val temp2 = new Variable("temp2")
    val temp4 = new Variable("temp4", isPointer = true)
    val f = new Procedure("f", Seq(temp1))
    val g = new Procedure("g", Seq(temp2), outer = Some(f))
    val h = new Procedure("h", Seq(temp4), outer = Some(g))
    f.code = call(g, const(2))
    g.code = Closure(h)
    h.code = binOp(v(temp1), plus, v(temp4))
    val a = new Variable("a")
    val b = new Variable("b")
    val main = new Procedure("main", Seq(a, b))
    main.code = CallClosure(call(f, const(4)), Seq(CallClosure(call(f, const(4)), Seq(const(1)), Seq(new Variable("p", isPointer = true)))), Seq(new Variable("p", isPointer = true)))
    val machineCode = compilerA6(Seq(main, f,g, h))
    val endState = A4.loadAndRun(machineCode, debug = true)
    println(decodeSigned(endState.reg(3)))
  }

  test("new"){

  }

  test("lacs") {
    val w = new Variable("w")
    val temp1 = new Variable("temp1")
    val temp2 = new Variable("temp2")
    val temp3 = new Variable("temp3")
    val temp4 = new Variable("temp4")
    val f = new Procedure("f", Seq(temp1, temp2))
    val h = new Procedure("h", Seq(temp3), outer = Some(f))

    val q = new Procedure("q", Seq(), outer = Some(h))
    val k = new Procedure("k", Seq(temp4), outer = Some(q))
    val g = new Procedure("g", Seq(), outer = Some(q))
    f.code = Scope(Seq(w), block(
      call(h, const(4))
    ))
    h.code = Scope(Seq(w),call(q))
    k.code = ifStmt(v(temp1), eqCmp, const(5), const(8), binOp(v(temp1), plus, call(g)))
    q.code = call(g)
    g.code =block(
      assign(temp1, binOp(v(temp1), plus, const(1))),
      call(k, const(16))
    )
    val a = new Variable("a")
    val b = new Variable("b")
    val main = new Procedure("main", Seq(a, b))
    main.code = call(f, const(3), const(0))

    val machineCode = compilerA6(Seq(main, f, h, k,q,g))
    val endState = A4.loadAndRun(machineCode, debug = true)
    println(decodeSigned(endState.reg(3)))
  }
  test("lacs2") {
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

    val machineCode = compilerA6(Seq(main, fact))
    val endState = A4.loadAndRun(machineCode, debug = true)
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
      call(addOne, const(1))
    ))
    val machineCode = compilerA6(Seq(main, addOne))
    val endState = A4.loadAndRun(machineCode, Word(encodeSigned(-2)), Word(encodeSigned(-4)), debug = true)
    println(decodeSigned(endState.reg(3)))
  }
  test("scala_closures") {
    def increaseBy(increment: Int): (Int)=>Int = {
      def procedure(x: Int) = { x + increment }
      procedure
    }
    def main(a: Int, b: Int) = {
      val proc = increaseBy(a)
      (proc)(b) + (increaseBy(2*b))(3*a)
    }
    println(main(3,5))
  }

  test("2"){
    /*
    def foo(free: Int): (Int=>Int)=>Int{
      def increaseBy(increment: Int): (Int)=>Int = {
      def procedure(x: Int) = { x + increment }
      procedure
    }
    increaseBy
    }*/
    val temp1 = new Variable("temp1")
    val temp2 = new Variable("temp2")
    val temp3 = new Variable("temp3")
    val temp4 = new Variable("temp4", isPointer = true)
    val foo = new Procedure("foo", Seq(temp1))
    val increaseBy = new Procedure("increaseBy", Seq(temp2), outer = Some(foo))
    val procedure = new Procedure("proc", Seq(temp3, new Variable("temp")), outer = Some(increaseBy))
    val a = new Variable("a")
    val b = new Variable("b")
    val main = new Procedure("main", Seq(a, b))
    procedure.code = binOp(binOp(v(temp1), plus, v(temp2)), plus, v(temp3))
    increaseBy.code = block(
      //assign(temp2, const(4)),
      Closure(procedure)
    )
    foo.code = Closure(increaseBy)
    main.code = Scope(Seq(temp4), block(
      //foo(3)(foo(3)(4)(5))(6)
      //CallClosure(CallClosure(call(foo, const(3)), Seq(const(4)), Seq(new Variable("new"))), Seq(const(6)), Seq(new Variable("new")))
      CallClosure(CallClosure(call(foo, const(3)), Seq(CallClosure(CallClosure(call(foo, const(3)), Seq(const(4)), Seq(new Variable("new"))), Seq(const(6)), Seq(new Variable("new")))), Seq(new Variable("new"))), Seq(const(6)), Seq(new Variable("new")))
      //assign(temp4, call(foo, const(3))),
      //CallClosure(v(temp4), Seq( CallClosure(v(temp4), Seq(const(5), const(1)), Seq(new Variable("new"),new Variable("new"))),const(1)), Seq(new Variable("new"),new Variable("new")))
    ))
    val machineCode = compilerA6(Seq(main, foo, increaseBy, procedure))
    val endState = A4.loadAndRun(machineCode, Word(encodeSigned(-2)), Word(encodeSigned(-4)), debug = true)
    println(decodeSigned(endState.reg(3)))
  }

  test("3"){
    /*
    def foo(free: Int): (Int=>Int)=>Int{
      def increaseBy(increment: Int): (Int)=>Int = {
      def procedure(x: Int) = { x + increment }
      procedure
    }
    increaseBy
    }*/
    val temp1 = new Variable("temp1")
    val temp2 = new Variable("temp2")
    val temp3 = new Variable("temp3")
    val temp4 = new Variable("temp4", isPointer = true)
    val foo = new Procedure("foo", Seq(temp1))
    val increaseBy = new Procedure("increaseBy", Seq(temp2), outer = Some(foo))
    val procedure = new Procedure("proc", Seq(temp3, new Variable("temp")), outer = Some(increaseBy))
    val a = new Variable("a")
    val b = new Variable("b")
    val main = new Procedure("main", Seq(a, b))
    procedure.code = block(
      assign(temp3, binOp(v(temp3), plus, const(1))),
      binOp(binOp(v(temp1), plus, v(temp2)), plus, v(temp3))
    )
    increaseBy.code = block(
      //assign(temp2, const(4)),
      Closure(procedure)
    )
    foo.code = Closure(increaseBy)
    main.code = Scope(Seq(temp4), block(
      assign(temp4,CallClosure(call(foo, const(3)), Seq(const(4)), Seq(new Variable("new"))) ),
      CallClosure(v(temp4),Seq(const(6)), Seq(new Variable("new")) )
      //foo(3)(foo(3)(4)(5))(6)
      //CallClosure(CallClosure(call(foo, const(3)), Seq(const(4)), Seq(new Variable("new"))), Seq(const(6)), Seq(new Variable("new")))
      //CallClosure(CallClosure(call(foo, const(3)), Seq(CallClosure(CallClosure(call(foo, const(3)), Seq(const(4)), Seq(new Variable("new"))), Seq(const(6)), Seq(new Variable("new")))), Seq(new Variable("new"))), Seq(const(6)), Seq(new Variable("new")))
      //assign(temp4, call(foo, const(3))),
      //CallClosure(v(temp4), Seq( CallClosure(v(temp4), Seq(const(5), const(1)), Seq(new Variable("new"),new Variable("new"))),const(1)), Seq(new Variable("new"),new Variable("new")))
    ))
    val machineCode = compilerA6(Seq(main, foo, increaseBy, procedure))
    val endState = A4.loadAndRun(machineCode, Word(encodeSigned(-2)), Word(encodeSigned(-4)), debug = true)
    println(decodeSigned(endState.reg(3)))
  }

  test("lacs_closures") {
    val increment = new Variable("increment")
    val x = new Variable("x")
    val proc = new Variable("proc", isPointer = true)
    val increaseBy = new Procedure("increaseBy", Seq(increment))
    val procedure = new Procedure("procedure", Seq(x), outer = Some(increaseBy))
    increaseBy.code = Closure(procedure)
    procedure.code = binOp(v(x), plus, v(increment))

    val p1 = new Variable("p1")

    val a = new Variable("a")
    val b = new Variable("b")
    val main = new Procedure("main", Seq(a, b))
    main.code = Scope(Seq(proc), block(
      assign(proc, call(increaseBy, v(a))),
      binOp(
        CallClosure(v(proc),Seq(v(b)),Seq(p1)),
        plus,
        CallClosure(call(increaseBy, binOp(const(2), times, v(b))),
          Seq(binOp(const(3), times, v(a))),
          Seq(p1)
        )
      )
    ))

    val machineCode = compilerA6(Seq(main, increaseBy, procedure))
    val endState = A4.loadAndRun(machineCode,
      register1 = Word(encodeSigned(3)),
      register2 = Word(encodeSigned(5)),
      debug = true)
    println(decodeSigned(endState.reg(3)))

    // additional code patterns to include in tests
    //    assign(proc, CallClosure(Closure(increaseBy), Seq(v(a)), Seq(p1)))
    //    CallClosure(CallClosure(???, ???, ???), ???, ???)
    //    CallClosure(???, Seq(CallClosure(???, ???, ???)), ???)
    //    CallClosure(???, Seq(const(42), CallClosure(???, ???, ???)), ???)
  }
  test("lacs_closures2") {
    val increment = new Variable("increment")

    val x = new Variable("x")
    val y = new Variable("x")
    val proc = new Variable("proc", isPointer = true)
    val z = new Variable("proc2", isPointer = true)
    val increaseBy = new Procedure("increaseBy", Seq(increment))
    val procedure = new Procedure("procedure", Seq(x), outer = Some(increaseBy))
    val procedure2 = new Procedure("procedure2", Seq(y), outer = Some(increaseBy))
    val procedure3 = new Procedure("procedure2", Seq(z), outer = Some(increaseBy))
    increaseBy.code = call(procedure2, const(0))
    procedure.code = binOp(v(x), plus, v(increment))
    procedure2.code = Closure(procedure)
    val p1 = new Variable("p1")

    val a = new Variable("a")
    val b = new Variable("b")
    val main = new Procedure("main", Seq(a, b))
    main.code = Scope(Seq(proc), block(
      assign(a, const(6)),
      assign(proc, call(increaseBy, v(a))),
      CallClosure(block(
        CallClosure(v(proc), Seq(const(1)), Seq(p1)),
        v(proc)
      ),Seq(CallClosure(v(proc), Seq(const(1)), Seq(p1))),Seq(p1))
    ))

    val machineCode = compilerA6(Seq(main, increaseBy, procedure, procedure2))
    val endState = A4.loadAndRun(machineCode,
      register1 = Word(encodeSigned(3)),
      register2 = Word(encodeSigned(6)),
      debug = true)
    println(decodeSigned(endState.reg(3)))

    // additional code patterns to include in tests
    //    assign(proc, CallClosure(Closure(increaseBy), Seq(v(a)), Seq(p1)))
    //    CallClosure(CallClosure(???, ???, ???), ???, ???)
    //    CallClosure(???, Seq(CallClosure(???, ???, ???)), ???)
    //    CallClosure(???, Seq(const(42), CallClosure(???, ???, ???)), ???)
  }

  test("tailcall"){
    val a = new Variable("a")
    val b = new Variable("b")
    val c = new Variable("c")
    val main = new Procedure("main", Seq(a, b))
    val f = new Procedure("f", Seq(c), outer = Some(main))
    f.code = block(v(a))
    main.code = block(
      Comment("I am reading param"),
      read(Reg.scratch, main.paramPtr),
      call(f, const(2)),
      v(a)
    )
    val machineCode = compilerA6(Seq(main, f))
    val endState = A4.loadAndRun(machineCode,
      register1 = Word(encodeSigned(3)),
      register2 = Word(encodeSigned(6)),
      debug = true)
    println(decodeSigned(endState.reg(3)))
  }

  test("moreclosure"){
    val a = new Variable("a")
    val b = new Variable("b")
    val c = new Variable("c")
    val d = new Variable("d")
    val e = new Variable("e")
    val proc1 = new Variable("proc", isPointer = true)
    val main = new Procedure("main", Seq(a, b))
    val f = new Procedure("f", Seq(c), outer = Some(main))
    val h = new Procedure("h", Seq(new Variable("temp",isPointer = true)), outer = Some(f))
    val i = new Procedure("i", Seq(new Variable("temp")), outer = Some(h))
    val j = new Procedure("j", Seq(new Variable("temp",isPointer = true)), outer = Some(i))
    val g = new Procedure("g",Seq(d,e), outer = Some(f))
    main.code = Scope(Seq(proc1), block(
      assign(proc1, CallClosure(Closure(f), Seq(const(1)), Seq(new Variable("temp")))),
      CallClosure(read(Reg.result, proc1), Seq(const(3), const(4)), Seq(new Variable("temp"), new Variable("temp2")))
    ))
    f.code = block(
      call(h, const(0))
    )
    h.code = call(i, const(0))
    i.code = call(j, const(1))
    g.code = block(
     binOp( binOp(v(d),plus, v(e)), plus, v(a))
    )
    j.code = block(
      assign(a, binOp(read(Reg.result,a), plus, v(c))),
      Closure(g)
    )
    val machineCode = compilerA6(Seq(main, f,g,h,i,j))
    val endState = A4.loadAndRun(machineCode,
      register1 = Word(encodeSigned(3)),
      register2 = Word(encodeSigned(6)),
      debug = true)
    println(decodeSigned(endState.reg(3)))
  }

  test("final2"){
    val x = new Variable("x")
    val y = new Variable("y")
    val a = new Variable("a")
    val n = new Variable("n")
    val k = new Variable("k", isPointer = true)
    val b = new Variable("b")
    val c = new Variable("c", isPointer = true)
    val main = new Procedure("main", Seq(x,y))
    val foo = new Procedure("foo", Seq(a,b))
    val bar = new Procedure("bar", Seq())
    val closure = new Procedure("closure", Seq(c), outer = Some(foo))
    closure.code = binOp(v(a), plus,CallClosure(v(c), Seq(), Seq()))
    foo.code = Closure(closure)
    main.code = CallClosure(call(foo, const(1), const(2)), Seq(Closure(bar)), Seq(new Variable("bb", isPointer = true)))
    bar.code = const(10)
    val machineCode = compilerA6(Seq(main, foo, bar, closure))
    val endState = A4.loadAndRun(machineCode,
      debug = true)
    println(decodeSigned(endState.reg(3)))
  }

  test("final"){
    val x = new Variable("x")
    val y = new Variable("y")
    val a = new Variable("a")
    val n = new Variable("n")
    val k = new Variable("k", isPointer = true)
    val b = new Variable("b")
    val bb = new Variable("bb")

    val main = new Procedure("main", Seq(x,y))
    val f = new Procedure("f", Seq(a), Some(main))
    val g = new Procedure("g", Seq(b), Some(f))
    val h = new Procedure("h", Seq(), Some(f))

    g.code = v(a)
    h.code = Closure(g)
    f.code = Scope(Seq(k,n),block(
      assign(k, call(h)),
      assign(n, binOp(CallClosure(v(k), Seq(const(5)), Seq(bb)), plus,CallClosure(v(k), Seq(const(5)), Seq(bb)) )),
      Comment("Something wrong here"),
      binOp(v(n), plus, binOp(CallClosure(v(k), Seq(const(5)), Seq(bb)), plus, CallClosure(v(k), Seq(const(5)), Seq(bb))))
      //v(n),
      //binOp(CallClosure(v(k), Seq(const(5)), Seq(bb)), plus, CallClosure(v(k), Seq(const(5)), Seq(bb))),
      //binOp(v(n), plus,CallClosure(v(k), Seq(const(5)), Seq(bb)))
    ))
    main.code = block(
      call(f, const(42))
    )
    val machineCode = compilerA6(Seq(main, f,g,h))
    val endState = A4.loadAndRun(machineCode,
      debug = true)
    println(decodeSigned(endState.reg(3)))
  }


}
