package cs241e.assignments
import ProgramRepresentation._
import Assembler._
import cs241e.mips._
import cs241e.Utils._
import CodeBuilders._
object tools {
  /** multiply the value stored in s to const and store the result in d */
  def multiply(d: Reg, s: Reg, const: Int): Code= block(
      LIS(d),
      Word(encodeSigned(const)),
      MULT(d,s),
      MFLO(d)
    )

  /** store the sum of const and value stored in s to d  */
  def addI(d:Reg, s: Reg, const: Int): Code=block(
    LIS(Reg.scratch),
    Word(encodeSigned(const)),
    ADD(d, s, Reg.scratch)
  )

  def subI(d:Reg, s: Reg, const: Int): Code=block(
    LIS(Reg.scratch),
    Word(encodeSigned(const)),
    SUB(d, s, Reg.scratch)
  )

  /** store the quotation or remainder of (s divide by const) */
  def div(d:Reg, s:Reg, const: Int, isQuotation: Boolean): Code = {
    var body = Seq[Code](
      LIS(d),
      Word(encodeSigned(const)),
      DIV(s,d)
    )
    if (isQuotation){
      body = body.appended(MFLO(d))
    } else {
      body = body.appended(MFHI(d))
    }
    Block(body)
  }

  def const(c: Int) = block(
    LIS(Reg.result),
    Word(encodeSigned(c))
  )

  def generatrArray(array: Seq[Int]): Seq[Word]={
    def transform(num: Int): Word = Word(encodeSigned(num))
    array.map(transform)
  }

  /** Add two variables and store the result in Reg.result */
  def varADD(variable1: Variable, variable2: Variable): Code={
    block(
      read(Reg.result, variable1),
      read(Reg.scratch, variable2),
      ADD(Reg.result, Reg.result,Reg.scratch)
    )
  }

  /** Sub two variables and store the result in Reg.result */
  def varSUB(variable1: Variable, variable2: Variable): Code={
    block(
      read(Reg.result, variable1),
      read(Reg.scratch, variable2),
      SUB(Reg.result, Reg.result,Reg.scratch)
    )
  }

  /** mult two variables and store the result in Reg.result */
  def varMULT(variable1: Variable, variable2: Variable): Code={
    block(
      read(Reg.result, variable1),
      read(Reg.scratch, variable2),
      MULT(Reg.result,Reg.scratch),
      MFLO(Reg.result)
    )
  }

  /** div two variables and store the result in Reg.result */
  def varDIV(variable1: Variable, variable2: Variable, isQuotation: Boolean): Code={
    var ans = Seq[Code](
      read(Reg.result, variable1),
      read(Reg.scratch, variable2),
      DIV(Reg.result, Reg.scratch)
    )
    if (isQuotation) ans = ans.appended(MFLO(Reg.result))
    else ans = ans.appended(MFHI(Reg.result))
    Block(ans)
  }

  /** store exp(v1,v2) to Reg.result */
  def EXP(variable1: Variable, variable2: Variable): Code={
    val ans = new Variable("ans")
    val temp = new Variable("temp")
    Scope(Seq( ans, temp), block(
      assign(ans, const(1)),
      read(Reg.result, variable2),
      write(temp, Reg.result),
      whileLoop(read(Reg.result, temp), neCmp, const(0), block(
        binOp(read(Reg.result, ans), times, read(Reg.result, variable1)),
        write(ans, Reg.result),
        read(Reg.result, temp),
        subI(Reg.result, Reg.result, 1),
        write(temp, Reg.result)
      )),
      read(Reg.result, ans)
    ))
  }

  /** increment variable by 1 v++*/
  def incVar(variable: Variable): Code= assign(variable, binOp(read(Reg.result, variable), plus, const(1)))

  /** decrement variable by 1*/
  def decVar(variable: Variable): Code = assign(variable, binOp(read(Reg.result, variable), minus, const(1)))

  /** change the variable by i*/
  def addVar(variable: Variable, i: Int): Code = assign(variable, binOp(read(Reg.result, variable), plus, const(i)))

  /** store decimal number -1 to Reg.result */
  def baseTen(variable: Variable): Code={

    val Ten = new Variable("ten")
    val len = new Variable("len")
    val end = new Label("end")
    Scope(Seq(Ten,len), block(
      assign(Ten,const(10)),
      assign(len, const(1)),
      ifStmt(read(Reg.result, variable), ltCmp, const(0),
        block(
          whileLoop(binOp(const(-1), times, EXP(Ten,len)), geCmp, read(Reg.result,variable),block(
            read(Reg.result, len),
            addI(Reg.result, Reg.result,1),
            write(len, Reg.result),
            binOp(read(Reg.result,len), neCmp(end),const(10))
          ))
        ),
        block(
          whileLoop(EXP(Ten,len), leCmp, read(Reg.result,variable),block(
            read(Reg.result, len),
            addI(Reg.result, Reg.result,1),
            write(len, Reg.result),
            binOp(read(Reg.result,len), neCmp(end),const(10))
          ))
        )),
      Define(end),
      read(Reg.result, len),
      subI(Reg.result, Reg.result,1)
    ))
  }



  /** store the max value of exp1 and exp2 into Reg3*/
  def maximum(exp1: Code, exp2: Code): Code={
    val temp1 = new Variable("temp1")
    val temp2 = new Variable("temp2")
    Scope(Seq(temp1, temp2), block(
      assign(temp1, exp1),
      assign(temp2, exp2),
      ifStmt(read(Reg.result, temp1), ltCmp, read(Reg.result, temp2), read(Reg.result, temp2), read(Reg.result, temp1))
    ))
  }

  /** store the value of current index of the array to Reg3 */
  def getArrVal(arr: Variable, currIndex: Variable): Code={
    block(
      binOp(read(Reg.result, arr), plus, binOp(read(Reg.result, currIndex), times, const(4))),
      LW(Reg.result, 0, Reg.result)
    )
  }

  /**
    *
  mapCode(code, {
          case call: Call => {
            if (!call.isTail){
              var tempVars = createTempVars(call.procedure.parameters)
              val len = call.arguments.length
              var storeCode = Seq[Code]()
              var copyCode = Seq[Code]()
              for (i <- 0 to len-1){
                storeCode = storeCode.appended(assign(tempVars(i), call.arguments(i)))
              }
              //val parameters = createTempVars(tempVars).appended(procedure.staticLink)
              val tempstatic = new Variable("tempstatic",isPointer = true)
              tempVars = tempVars.appended(tempstatic)
              for (i <- 0 to len){
                copyCode = copyCode ++ Seq(
                  Comment("I am copying " + tempVars(i).name),
                  read(Reg.scratch, tempVars(i)),
                  paramChunks.get(call.procedure).get.store(Reg.result, paramChunks.get(call.procedure).get.variables(i), Reg.scratch)
                )
              }
              var ans = Seq[Code](
                Block(storeCode),
                assign(tempstatic, computeStaticLink(call.procedure))
              )
              if (frameOnHeap.contains(procedure)) {
                ans = ans.appended(SimpleHeapAllocator.allocate(paramChunks.get(call.procedure).get))
              } else {
                ans = ans.appended(Stack.allocate(paramChunks.get(call.procedure).get))
              }
              ans = ans ++ Seq[Code](Block(copyCode),
                LIS(Reg.targetPC),
                Use(call.procedure.label),
                JALR(Reg.targetPC)
              )
              Scope(tempVars, Block(ans))
            }


            else {
              var tempVars = createTempVars(call.procedure.parameters)
              val len = call.arguments.length
              var storeCode = Seq[Code]()
              var copyCode = Seq[Code]()
              for (i <- 0 to len-1){
                storeCode = storeCode.appended(assign(tempVars(i), call.arguments(i)))
              }
              //val parameters = createTempVars(tempVars).appended(procedure.staticLink)
              val tempstatic = new Variable("tempstatic", isPointer = true)
              tempVars = tempVars.appended(tempstatic)
              val params = Chunk(tempVars)
              val params2 = Chunk(tempVars)
              for (i <- 0 to len){
                copyCode = copyCode ++ Seq(
                  Comment("I am copying " + tempVars(i).name),
                  read(Reg.scratch, tempVars(i)),
                  params.store(Reg.result, params.variables(i), Reg.scratch)
                )
              }
              var ans = Seq[Code](
                Comment("This is a tail call"),
                Block(storeCode),
                assign(tempstatic, computeStaticLink(call.procedure)),
              )
              if (frameOnHeap.contains(call.procedure)) {
                ans = ans ++ Seq(
                  SimpleHeapAllocator.allocate(paramChunks.get(call.procedure).get),
                  Block(copyCode),
                )
                if (!frameOnHeap.contains(procedure)) {
                  ans = ans ++ Seq(
                    Stack.pop,
                    Stack.pop
                  )
                }
                ans = ans ++ Seq[Code](
                  LIS(Reg.targetPC),
                  Use(call.procedure.label),
                  JR(Reg.targetPC)
                )
              } else {
                ans = ans ++ Seq(
                  Stack.allocate(params),
                  Block(copyCode),
                  ADD(Reg.scratchExtend3, Reg.result),
                  read(Reg.link, procedure.savedPC),
                  read(Reg.framePointer, procedure.dynamicLink),

                )
                if (!frameOnHeap.contains(procedure)){
                  ans = ans  ++ Seq(
                    Stack.pop,
                    Stack.pop,
                    Stack.pop,
                    LW(Reg.framePointer, 8, Reg.result),
                    Stack.allocate(params2),
                    LW(Reg.framePointer, 8, Reg.result),
                    copyChunk(Reg.result2, Reg.result),
                    ADD(Reg.result, Reg.result2),
                    LIS(Reg.targetPC),
                    Use(call.procedure.label),
                    JR(Reg.targetPC)
                  )
                } else {
                  ans = ans  ++ Seq(
                    Stack.allocate2(paramChunks.get(call.procedure).get),
                    LW(Reg.framePointer, 8, Reg.result),
                    copyChunk(Reg.result2, Reg.result),
                    ADD(Reg.scratchExtend3, Reg.result2),
                    Comment("I am testinb"),
                    LW(Reg.framePointer, 8, Reg.scratchExtend3),
                    LIS(Reg.targetPC),
                    Use(call.procedure.label),
                    JR(Reg.targetPC)
                  )
                }
              }
              Scope(tempVars, Block(ans))
            }
          }

          case callClosure: CallClosure => {
            if (callClosure.isTail) {

              var tempVars = createTempVars(callClosure.parameters)
              val len = callClosure.arguments.length
              var storeCode = Seq[Code]()
              var copyCode = Seq[Code]()
              for (i <- 0 to len - 1) {
                storeCode = storeCode.appended(assign(tempVars(i), callClosure.arguments(i)))
              }
              val tempstatic = new Variable("tempstatic", isPointer = true)
              val parameters = Chunk(callClosure.parameters :+ tempstatic)
              val tempAdd = new Variable("tempAdd")
              tempVars = tempVars.appended(tempstatic)
              for (i <- 0 to len) {
                copyCode = copyCode ++ Seq(
                  read(Reg.scratch, tempVars(i)),
                  Comment("I am passing parameter " + tempVars(i).name),
                  parameters.store(Reg.result, parameters.variables(i), Reg.scratch)
                )
              }
              var ans = Seq[Code](
                Block(storeCode),
                callClosure.closure,
                closureChunk.load(Reg.result, Reg.scratchExtend1, closureEnvironment),
                write(tempstatic, Reg.scratchExtend1),
                closureChunk.load(Reg.result, Reg.scratchExtend1, closureCode),
                write(tempAdd, Reg.scratchExtend1),
                read(Reg.targetPC, tempAdd),
                read(Reg.link, procedure.savedPC),
                read(Reg.framePointer, procedure.dynamicLink),
                SimpleHeapAllocator.allocate(parameters),
                Block(copyCode),
              )
              if (frameOnHeap.contains(procedure)){
                ans = ans ++ Seq(
                  Stack.pop,
                  Stack.pop
                )
              }
              ans = ans ++ Seq[Code](

                JR(Reg.targetPC)
              )
              Scope(tempVars.appended(tempAdd), Block(ans))
            } else {

              var tempVars = createTempVars(callClosure.parameters)
              val len = callClosure.arguments.length
              var storeCode = Seq[Code]()
              var copyCode = Seq[Code]()
              for (i <- 0 to len - 1) {
                storeCode = storeCode.appended(assign(tempVars(i), callClosure.arguments(i)))
              }
              val tempstatic = new Variable("tempstatic", isPointer = true)
              val parameters = Chunk(callClosure.parameters :+ tempstatic)
              val tempAdd = new Variable("tempAdd")
              tempVars = tempVars.appended(tempstatic)
              for (i <- 0 to len) {
                copyCode = copyCode ++ Seq(
                  read(Reg.scratch, tempVars(i)),
                  Comment("I am passing parameter " + tempVars(i).name),
                  parameters.store(Reg.result, parameters.variables(i), Reg.scratch)
                )
              }
              var ans = Seq[Code](
                Block(storeCode),
                callClosure.closure,
                closureChunk.load(Reg.result, Reg.scratchExtend1, closureEnvironment),
                write(tempstatic, Reg.scratchExtend1),
                closureChunk.load(Reg.result, Reg.scratchExtend1, closureCode),
                write(tempAdd, Reg.scratchExtend1)
              )

              ans = ans ++ Seq[Code](
                SimpleHeapAllocator.allocate(parameters),
                Block(copyCode),
                read(Reg.targetPC, tempAdd),
                JALR(Reg.targetPC)
              )
              Scope(tempVars.appended(tempAdd), Block(ans))
            }
          }
        })
    */

}
