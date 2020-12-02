import cs241e.assignments.Assembler._
import cs241e.assignments.CodeBuilders._
import cs241e.assignments.ProgramRepresentation._
import cs241e.assignments.Transformations._
import cs241e.assignments.{A4, A7, Lacs, Reg, Scanning}
import cs241e.mips.Word
import org.scalatest.FunSuite



class A7Tests extends FunSuite {
  def listDiff[A](list: List[A], rest: List[A]): List[A] =
    if (list eq rest) Nil
    else list.head :: listDiff(list.tail, rest)
  test("recognize") {
    //assert(Scanning.recognize(Lacs.dfa, "main".toList) == true)
    //assert(Scanning.recognize(Lacs.dfa, "ma3in".toList) == true)
    //assert(Scanning.recognize(Lacs.dfa, "3main".toList) == false)
    val a = List('a', 'b', 'c')
    println(a.mkString)
    val map1 = Map(
      ' ' -> "WHITESPACE",
      '1' -> "haha"
    )
    pprint.pprintln(map1.values.toSet)
    assert(Scanning.recognize(A7.notDiv3, "11".toList) == false)
    assert(Scanning.recognize(A7.notDiv3, "100".toList) == true)
  }
  test("simple"){
    assert(Scanning.recognize(A7.abcSubstring, "abca".toList) == true)
    val input = """
         def main(a: Int, b: Int): Int = {
           a + b
         }
         //hahahaheee
        """
    val tokens = Scanning.maximalMunchScan(Lacs.dfa, input)
    pprint.pprintln(tokens)
  }

  test("err"){
    val input =
      """
        def main(a 10a)
        """
    val tokens = Lacs.scan(input)
    pprint.pprintln(tokens)
  }

  test("scan") {
    val prog =
      """
         def main(a: Int, b: Int): Int = {
           a + b
         }
         =
         ==
         =main
        """
    val tokens = Scanning.maximalMunchScan(Lacs.dfa, prog)
    val tokens2 = Lacs.scan(prog)
    pprint.pprintln(tokens2)

    val badProg1 = "3main"
    val badProg2 = "00"
    val badProg3 = "==="
    val badProg4 = "===="
    assertThrows[RuntimeException](Lacs.scan(badProg1))
    assertThrows[RuntimeException](Lacs.scan(badProg2))
    assertThrows[RuntimeException](Lacs.scan(badProg3))
    assertThrows[RuntimeException](Lacs.scan(badProg4))
    //    assertThrows[RuntimeException](Lacs.scan(prog))
  }

  test("testset"){
    var set = Set(1,2,3)
    set = set.+(4)
    println(set)
  }
}