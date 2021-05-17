package simpret.interpreter

import simpret.parser._
import simpret.errors._


abstract case class EvaluationException(val message: String, val x: AST,
                                     private val cause: Throwable = None.orNull)
  extends Exception(message, cause)

final class VariableCapturedEvaluationException(val var_id: String, val subst_s: AST,
                                                private val cause: Throwable = None.orNull)
  extends EvaluationException("variable (" + var_id + ") has been captured during substitution", subst_s, cause)

object Interpreter {
  def errVarCap(var_id: String, x: AST) = throw new VariableCapturedEvaluationException(var_id, x)

  /* function for defining which AST elements are values */
  def isvalue(x: AST): Boolean = x match {
    case BoolLit(_) => true
    case IntLit(_) => true
    case LamExp(_,_,_) => true
    case NilExp(_) => true
    case ConsExp(eh, et) => isvalue(eh) & isvalue(et)
    case TupleExp(el) => el.forall(isvalue)
    case RecordExp(em) => em.values.forall(isvalue)
    case _ => false
  }

/* function for determining the free variables of an expression */
  def freevars (x: AST) : List[String] = {
    x match{
        case Variable(id) => List(id)    
        case IsZeroExp(e) => freevars(e)
        case AppExp(e1, e2) => freevars(e1)++freevars(e2)
        case CondExp(c, e1, e2) => freevars(c)++freevars(e1)++freevars(e2)
        case PlusExp(e1, e2) => freevars(e1)++freevars(e2)

        case LamExp(id, _, e) => freevars(e).filterNot( _ == id )

        case _ => List()
    }
  }

  /* function for carrying out a substitution */
  /* [x -> s] t */
  def substitute(t: AST, x: String, s: AST): AST = t match {
    case Variable(t1) if (t1==x) => s
    case Variable(t1) if (t1!=x) => t
    //case LamExp(t1, e) if (t1==x) => t
    //case LamExp(t1, e) if (t1!=x)&&(!freevars(s).contains(t1)) => LamExp(t1, substitute(e, x, s))
    //case LamExp(t1, e) if (t1!=x)&&(freevars(s).contains(t1)) => LamExp(identifier, substitute(alphaReduce(e, t1, identifier), x, s))
    case LamExp(t1, ty, e) if (t1==x) => t
    case LamExp(t1, ty, e) if (t1!=x)&&(!freevars(s).contains(t1)) => LamExp(t1, ty, substitute(e, x, s))
    case LamExp(t1, ty, e) if (t1!=x)&&(freevars(s).contains(t1)) => errVarCap(t1, s)

    case AppExp(e1, e2) => AppExp(substitute(e1, x, s), substitute(e2, x, s))
    case IsZeroExp(e) => IsZeroExp(substitute(e, x, s))
    case PlusExp(e1, e2) => PlusExp(substitute(e1, x, s), substitute(e2, x, s))
    case CondExp(c, e1, e2) => CondExp(substitute(c, x, s), substitute(e1, x, s), substitute(e2, x, s))

    case LtExp(e1, e2) => LtExp(substitute(e1, x, s), substitute(e2, x, s))
    case UMinExp(e) => UMinExp(substitute(e, x, s))
    case FixAppExp(e) => FixAppExp(substitute(e, x, s))
    case LetExp(id, e1, e2) if (!freevars(s).contains(id))&&(id==x) => LetExp(id, substitute(e1, x, s), e2)
    case LetExp(id, e1, e2) if (!freevars(s).contains(id))&&(id!=x) => LetExp(id, substitute(e1, x, s), substitute(e2, x, s))
    case LetExp(id, e1, e2) if (freevars(s).contains(id)) => errVarCap(id, s)

    case TupleExp(e) => TupleExp(e.map(esub => substitute(esub, x, s)))
    case ProjTupleExp(e, i) => ProjTupleExp(substitute(e, x, s), i)

    case _ => t
	}
/***
  def alphaReduce(t: AST, x: String, y: String): AST = t match {
    case Variable(t1) if (t1==x) => Variable(y)
	case Variable(t1) if (t1!=x) => t
    case IsZeroExp(e) => IsZeroExp(alphaReduce(e, x, y))
   	case AppExp(e1, e2) => AppExp(alphaReduce(e1, x, y), alphaReduce(e2, x, y))
    case CondExp(c, e1, e2) => CondExp(alphaReduce(c, x, y), alphaReduce(e1, x, y), alphaReduce(e2, x, y))
    case PlusExp(e1, e2) => PlusExp(alphaReduce(e1, x, y), alphaReduce(e2, x, y))
	case LamExp(id, _, t1) if (id==x) => t
	case LamExp(id, _, t1) if (id!=x) => LamExp(id, _, alphaReduce(t1, x, y))
    case _ => t
  }
***/

  /* evaluation function for taking one step at a time */
  def step(tree: AST): Option[AST] = {
    tree match {
	    case IsZeroExp(IntLit(x)) if (x==0) => Some(BoolLit(true))
	    case IsZeroExp(IntLit(x)) if (x!=0) => Some(BoolLit(false))
	    case IsZeroExp(x) if (!isvalue(x)) => step(x) match {
    	case None => None
    	case Some(x1) => Some(IsZeroExp(x1))
		  }

      case CondExp(BoolLit(true), y, z)  => Some(y)
      case CondExp(BoolLit(false), y, z) => Some(z)
      case CondExp(x, y, z) if (!isvalue(x)) => step(x) match {
    		case None => None
    		case Some(x1) => Some(CondExp(x1,y,z))
		  }

      case PlusExp(IntLit(x), IntLit(y)) =>Some(IntLit(x + y))
      case PlusExp(IntLit(x), y) if (!isvalue(y)) => step(y) match {
    		case None => None
    		case Some(y1) => Some(PlusExp(IntLit(x),y1))
		  }
	  case PlusExp(x, y) if (!isvalue(x)) => step(x) match {
    		case None => None
    		case Some(x1) => Some(PlusExp(x1,y))
	  }
	
	    case AppExp(x, y) if (!isvalue(x)) => step(x) match{
          case None => None
          case Some(x1) => Some(AppExp(x1, y))
		  }
        case AppExp(x, y) if (!isvalue(y)) => step(y) match{
        case None => None
        case Some(y1) => Some(AppExp(x, y1))
		  }
      case AppExp(LamExp(x, _, t), y) if (isvalue(y)) => Some(substitute(t, x, y))

      // E-LessThanLeft
      case LtExp(x, y) if !isvalue(x) => step(x) match {
        case Some(x1) => Some(LtExp(x1, y))
        case None => None
      }
      // E-LessThanRight
      case LtExp(x, y) if !isvalue(y) => step(y) match {
        case Some(y1) => Some(LtExp(x, y1))
        case None => None
      }
      // E-LessThan
      case LtExp(x, y) => (x,y) match {
        case (IntLit(x1), IntLit(y1)) => Some(BoolLit(x1 < y1))
        case _ => None
      }
      // E-Minus
      case UMinExp(x) if !isvalue(x) => step(x) match {
        case Some(x1) => Some(UMinExp(x1))
        case _ => None
      }
      // E-MinusVal
      case UMinExp(x) => x match {
        case IntLit(x1) => Some(IntLit(-x1))
        case _ => None
      }
      // E-Let
      case LetExp(id, x, y) if !isvalue(x) => step(x) match {
        case Some(x1) => Some(LetExp(id, x1, y))
        case None => None
      }
      // E-LetV
      case LetExp(id, x, y) => Some(substitute(x, id, y))
      // E-Fix
      case FixAppExp(x) if !isvalue(x) => step(x) match {
        case Some(x1) => Some(FixAppExp(x1))
        case None => None
      }
      // E-FixBeta
      case FixAppExp(x) => x match {
        case LamExp(id, t, e) => Some(substitute(e, id, tree))
        case _ => None
      }
      //Tuples
      case TupleExp(x) => {
        var i = 0
        while(isvalue(x(i))) i += 1
        step(x(i)) match {
          case Some(x1) => Some(TupleExp(x.take(i) ++ List(x1) ++ x.drop(i+1)))
        }
      }
      // E-ProjTuple
      case ProjTupleExp(TupleExp(x), i) => Some(x(i-1))
      // E-Proj
      case ProjTupleExp(x, y) if !isvalue(x) => step(x) match {
        case Some(x1) => Some(ProjTupleExp(x1, y))
        case _ => None
      }
      case _ => None
	  }
  }

  /* evaluation function to iterate the steps of evaluation */
  def eval(x: AST): AST = step(x) match {
    case None => x
    case Some(x1) => eval(x1)
  }

  /* function to apply the interpreter */
  def apply(x: AST): Either[EvaluationError, AST] = {
    try {
      Right(eval(x))
    } catch {
      case EvaluationException (msg, xe, _) =>
        val msg2 = msg + " -> \r\n" + ASTPrinter.convToStr(xe)
        Left(EvaluationError(Location.fromPos(xe.pos), msg2))
    }
  }
}
