package simpret


import simpret.lexer._
import simpret.parser._
import simpret.errors._
import simpret.interpreter._
import org.scalatest.FunSuite
import java.io.File


class MainTestInterpreter extends FunSuite {

  def test_eval(filename: String, expected_result: Either[InterpreterError, AST]) = {
    assert(runcase(filename) === expected_result)
  }

  def getListOfFiles(dir: String, extensions: List[String]): List[File] = {
    new File(dir).listFiles.filter(_.isFile).toList.filter { file =>
      extensions.exists(file.getName.endsWith(_))
    }
  }

  def loadAst(filename: String): Either[InterpreterError, AST] = {
    for {
      input <- FileLoader(filename).right
      tokens <- Lexer(input).right
      ast <- Parser(tokens).right
    } yield (ast)
  }

  def singleAutoTest(in: File) = {
    var inFilename = in.getAbsolutePath()
    var stepFilename = inFilename + ".step"
    var ioError = true
    for {
      inAst <- loadAst(inFilename).right
      stepAst <- loadAst(stepFilename).right
    } yield {
      assert(Interpreter.step(inAst) == Some(stepAst))
      ioError = false
    }
    assert(!ioError)
  }

  def fixAutoTest(in:File) = {
    var inFilename = in.getAbsolutePath()
    var fixFilename = inFilename + ".fix"
    var ioError = true
    for {
      inAst <- loadAst(inFilename).right
      fixAst <- loadAst(fixFilename).right
    } yield {
      ioError = false
      assert(Interpreter.eval(inAst) == fixAst)
    }
    assert(!ioError)
  }

  def runstep(filename: String): Either[InterpreterError, Option[AST]] = {
    for {
      ast <- loadAst(filename).right
    } yield Interpreter.step(ast)
  }

  def runcase(filename: String): Either[InterpreterError, AST] = {
    for {
      ast <- loadAst(filename).right
    } yield (Interpreter.eval(ast))
  }

}
