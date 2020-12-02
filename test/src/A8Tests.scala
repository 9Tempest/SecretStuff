import cs241e.assignments.{A7, Lacs, Parsing, Scanning, Typer}
import cs241e.scanparse.DFAs.Token
import cs241e.scanparse.Grammars
import org.scalatest.FunSuite

class A8Tests extends FunSuite {
  test("simple-grammar") {
    val grammar = Grammars.parseGrammar(
      """
         expr expr op expr
         expr ID
         op +
         op *
        """
    )
    /*
    println(grammar.nonTerminals)
    println(grammar.terminals)
    println(grammar.productions)
    println(grammar.start)
    println(grammar.productionsExpanding("expr"))*/

    val badtokens = Seq(Token("ID", "x"), Token("+"), Token("ID"), Token("*"))
    val tokens = Seq(Token("ID", "x"), Token("+"), Token("ID"), Token("*"), Token("ID"))
    //println(tokens(3)
    def kinds(ts: Seq[Token]) = ts.map(token => token.kind)
    assert(Parsing.parseEarley(grammar, kinds(tokens).toIndexedSeq) == true)
    println("haha")
    println(kinds(badtokens).toIndexedSeq)
    assert(Parsing.parseEarley(grammar, kinds(badtokens).toIndexedSeq) == false)
  }
  test("lacs") {
    //    println(Lacs.grammar)
    val prog =
      """

         def main(a: Int, b: (Int, (Int)=>Int)=>Int): Int = {
           var c : Int;
           def helper(a: Int): Int = {
            c
           }
           if (1 > 1){
           11
           } else {
           10
           }
         }

        """

    val tokens = Lacs.scan(prog)
    pprint.pprintln(Typer.collect(Parsing.parseCYK(Lacs.grammar,tokens.toIndexedSeq).get, "expras"))
    pprint.pprintln(Typer.parseVarDefs(Parsing.parseCYK(Lacs.grammar,tokens.toIndexedSeq).get.children(1).children.head.children(3)))
   // println(Typer.collect(Parsing.parseCYK(Lacs.grammar,tokens.toIndexedSeq).get, "vardef").map(x=>Typer.collect(x, "type")))
    //pprint.pprintln(Typer.parseVarDefs(Parsing.parseCYK(Lacs.grammar,tokens.toIndexedSeq).get.children(1).children(3)))

  }

  test("lacs2") {

  }

}