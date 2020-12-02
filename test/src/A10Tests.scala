import cs241e.assignments.Assembler.{decodeSigned, encodeSigned}
import cs241e.assignments.{A4, CodeGenerator, Lacs, Typer}
import cs241e.mips.Word
import cs241e.scanparse.Grammars.Tree
import org.scalatest.FunSuite

class A10Tests extends FunSuite {
  test("test") {
    val prog =
      """def main(a: Int, b: Int): Int = {
           var q : ()=>Int;
           def foo() : ()=>Int = {
             q
           }
           main(main(a,b), main(main(a,b), foo()()))
    }
        def foo() : Int = {
        11
        }
        """
    val tree = Lacs.scanAndParse(prog)
    val typedProcedures = Typer.typeTree(tree)
    pprint.pprintln(typedProcedures)
    // Add the following line inside ProcedureScope
    //    override def toString = "ProcedureScope(" + name + ")"
    def getType(tree: Tree) = try { typedProcedures.typeMap(tree) } catch { case _ => "" }
    def show(tree: Tree, indent: Int = 0): String =
      " " * indent + tree.lhs + ": " + getType(tree) + "\n" +
        tree.children.map(ch => show(ch, indent+1)).mkString

    println(show(tree))

    println(show(typedProcedures.procedureScopes(0).expras))
  }


  test("1"){
    val machineCode1 = Lacs.compile(
      """
        def main(a: Int, b: Int) :Int = {
        var c : ()=>Int;
        def helper(a: Int) :Int = {a}
        a = 10;

        foo(1,2)(bar)
        }
        def foo(a: Int, b: Int): (()=>Int)=> Int = {
          def closure(c: ()=>Int): Int = {
            a + b + c()
          }
          closure
        }
        def bar(): Int = {
        10
        }
        """)
    val endState = A4.loadAndRun(machineCode1,
      register1 = Word(encodeSigned(3)),
      register2 = Word(encodeSigned(6)),debug = true)
    println(decodeSigned(endState.reg(3)))
  }
  test("2"){
    val machineCode1 = Lacs.compile(
      """
        def main(a: Int, b: Int) :Int = {
        var c : ()=>Int;
        def helper(a: Int) :Int = {a}
        a = 10;

        foo(1,2)()
        }
        def foo(a: Int, b: Int): ()=> Int = {
          def closure(): Int = {
            a + b + bar()
          }
          closure
        }
        def bar(): Int = {
        10
        }
        """)
    val ans1 = Lacs.scanAndParseAndType("""
        def main(a: Int, b: Int) :Int = {
        var c : ()=>Int;
        def helper(a: Int) :Int = {a}
        a = 10;

        foo(1,2)()
        }
        def foo(a: Int, b: Int): ()=> Int = {
          def closure(): Int = {
            a + b + bar()
          }
          closure
        }
        def bar(): Int = {
        10
        }
        """)
    pprint.pprintln(ans1.procedureScopes.map(x=>x.parms))
    val endState = A4.loadAndRun(machineCode1,
      register1 = Word(encodeSigned(3)),
      register2 = Word(encodeSigned(6)),debug = true)
     println(decodeSigned(endState.reg(3)))
  }

  test("3"){
    val machineCode = Lacs.compileWithGarbageCollector(
      """
        def main(a:Int, b:Int): Int = {
        (foo)()
        }
        def foo(): Int = {10}
        """)

    val endState = A4.loadAndRun(machineCode,
      register1 = Word(encodeSigned(3)),
      register2 = Word(encodeSigned(6)),debug = true)
    println(decodeSigned(endState.reg(3)))
  }


}