package simpret.typechecker

import simpret.parser._
import simpret.errors._

object Typechecker {
  // error handling helper functions
  def errUnknownAST(x: AST) = throw new UnknownASTTypecheckerException(x)
  def errExpectedType(ty_str: String, x: AST) = throw new NotExpectedTypeTypecheckerException(ty_str, x)
  def errVarUnbound(x: AST) = throw new VarUnboundTypecheckerException(x)
  def errAppArgument(ty_param: ASTTY, ty_arg: ASTTY, x: AST) = throw new ApplicationArgumentTypecheckerException(ty_param, ty_arg, x)
  def errBranch(ty1: ASTTY, ty2: ASTTY, x: AST) = throw new BranchMismatchTypecheckerException(ty1, ty2, x)
  def errArrowNotSame(ty_param: ASTTY, ty_res: ASTTY, x: AST) = throw new ArrowNotSameTypecheckerException(ty_param, ty_res, x)
  def errCons(eh_ty: ASTTY, et_lty: ASTTY, x: AST) = throw new ConsMismatchTypecheckerException(eh_ty, et_lty, x)
  def errProjTooSmall(x: AST) = throw new ProjectionTooSmallTypecheckerException(x)
  def errProjTooBig(length: Int, x: AST) = throw new ProjectionTooBigTypecheckerException(length, x)
  def errProjNotField(l: String, x: AST) = throw new ProjectionNotAFieldTypecheckerException(l, x)

  // the recursive typechecking relation
  def check(x: AST, env: Map[String, ASTTY] = Map.empty):ASTTY = {
    x match {

        case Variable(id) => env.get(id) match {
        case Some(t) => t
        case None => errVarUnbound(x)
        }

        case BoolLit(_) => BoolTy
        case IntLit(x) => IntTy

        case CondExp(e1,e2,e3) => check(e1, env) match {
        case BoolTy => {
          val t2 = check(e2, env)
          val t3 = check(e3, env)
          if (t2 == t3) t2
          else errBranch(t2, t3, x)
        }
        case _ => errExpectedType("Boolean", x)
        }

        case PlusExp(e1, e2) => (check(e1, env), check(e2, env)) match {
          case (IntTy, IntTy) => IntTy
          case _ => errExpectedType("IntTy", x)
        }
        case LtExp(e1, e2) => (check(e1, env), check(e2, env)) match {
          case (IntTy, IntTy) => BoolTy
          case _ => errExpectedType("IntTy", x)
        }
        case UMinExp(e1) => check(e1, env) match {
          case IntTy => IntTy
          case _ => errExpectedType("IntTy", x)
        }
        case LamExp(id, ty, e) => ArrowTy(ty, check(e,(env + (id -> ty))))

        case AppExp(e1, e2) => check(e1, env) match {
          case ArrowTy(t11, t12) => {
            var t2 = check(e2, env)
            if (t11 == t2) t12
            else errAppArgument(ArrowTy(t11, t12), t2, x)
          }
          case _ => errExpectedType("T11 -> T12", x)
        }

        case LetExp(id, e1, e2) => {
          var t1 = check(e1, env)
          var env_x = env + (id -> t1)
          check(e2, env_x)
        }

        case FixAppExp(e) => check(e, env) match {
          case ArrowTy(t1, t2) =>  {
            if (t1 == t2) t1
            else errArrowNotSame(t1, t2, x)
          }
          case _ => errExpectedType("ArrowTy(t, t)", x)
        }




    }
  }

  /* function to apply the interpreter */
  def apply(x: AST): Either[TypecheckingError, Unit] = {
    try {
      check(x)
      Right(Unit)
    } catch {
      case ex: TypecheckerException =>
        val msg = ex.message
        val x = ex.x
        val msg2 = msg + " -> \r\n" + ASTPrinter.convToStr(x)
        Left(TypecheckingError(Location.fromPos(x.pos), msg2))
    }
  }
}
