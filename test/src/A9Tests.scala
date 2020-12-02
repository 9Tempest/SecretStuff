import cs241e.assignments.{Lacs, Parsing, Typer}
import cs241e.scanparse.DFAs.Token
import cs241e.scanparse.Grammars
import cs241e.scanparse.Grammars.Tree
import org.scalatest.FunSuite

class A9Tests extends FunSuite {
  test("test") {
    val prog =
      """def main(a: Int, b: Int): Int = {
           var q : ()=>Int;
           def haha() : ()=>Int = {
             q
           }
           main(main(a,b), main(main(a,b), haha()()))
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

    pprint.pprintln(typedProcedures.procedureScopes.map(x=>x.procedure.outer))
  }
  test("testing") {
    def bad(prog: String) =
      assertThrows[RuntimeException](Lacs.scanAndParseAndType(prog))
    def good(prog: String) = Lacs.scanAndParseAndType(prog)

    bad("def main(a: Int, b: Int): Int = { a + c }")
    bad("def main(a: Int, a: Int): Int = { a }")
    good("def main(a: Int, b: Int): Int = { main(1,2) }")
    bad("def main(a: Int, b: Int): Int = { var d: ()=>Int; d = a; d }")
    good("""def main(a: Int, b: Int): Int = {
           var q : ()=>Int;
           def foo() : ()=>Int = {
             q = foo();
              (q)
           }
           main(main(a,b), main(main(a,b), foo()))
    }
        def foo() : Int = {
        11
        }
        """)
  }
}