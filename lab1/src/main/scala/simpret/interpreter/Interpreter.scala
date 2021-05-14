package simpret.interpreter

import simpret.parser._
import simpret.errors._


case class EvaluationException(private val message: String, private val x: AST,
  private val cause: Throwable = None.orNull)
    extends Exception(message, cause)


object Interpreter {
  def errFun(msg: String, x: AST) = throw new EvaluationException(msg, x)

  /* function for defining which AST elements are values */
  def isvalue(x: AST) = x match {
    case BoolLit(_) => true
    case IntLit(_) => true
    case LamExp(_, _) => true
    case _ => false
  }

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
	case AppExp(LamExp(x, t), y) if (isvalue(y)) => Some(substitute(t, x, y)) 
		
	case _ => None
	}
  }

val identifier : String = "newidentifier"

 def substitute(t: AST, x: String, y: AST): AST = t match {
	case Variable(t1) if (t1==x) => y
	case Variable(t1) if (t1!=x) => t
	case LamExp(t1, e) if (t1==x) => t
	case LamExp(t1, e) if (t1!=x)&&(!freevars(y).contains(t1)) => LamExp(t1, substitute(e, x, y))
	case LamExp(t1, e) if (t1!=x)&&(freevars(y).contains(t1)) => LamExp(identifier, substitute(alphaReduce(e, t1, identifier), x, y))
	case AppExp(e1, e2) => AppExp(substitute(e1, x, y), substitute(e2, x, y))
	case IsZeroExp(e) => IsZeroExp(substitute(e, x, y))
	case PlusExp(e1, e2) => PlusExp(substitute(e1, x, y), substitute(e2, x, y))
	case CondExp(c, e1, e2) => CondExp(substitute(c, x, y), substitute(e1, x, y), substitute(e2, x, y))
	case _ => t
	}

/* function for determining the free variables of an expression */
def freevars (x: AST) : List[String] = {
  x match{
    	case Variable(id) => List(id)    
    	case IsZeroExp(e) => freevars(e)
   	case AppExp(e1, e2) => freevars(e1)++freevars(e2)
    	case CondExp(c, e1, e2) => freevars(c)++freevars(e1)++freevars(e2)
    	case PlusExp(e1, e2) => freevars(e1)++freevars(e2)
	case LamExp(id, t) => freevars(t).filterNot( _ == id )
    	case _ => List()
  }
}

def alphaReduce(t: AST, x: String, y: String): AST = t match {
    	case Variable(t1) if (t1==x) => Variable(y)
	case Variable(t1) if (t1!=x) => t
    	case IsZeroExp(e) => IsZeroExp(alphaReduce(e, x, y))
   	case AppExp(e1, e2) => AppExp(alphaReduce(e1, x, y), alphaReduce(e2, x, y))
    	case CondExp(c, e1, e2) => CondExp(alphaReduce(c, x, y), alphaReduce(e1, x, y), alphaReduce(e2, x, y))
    	case PlusExp(e1, e2) => PlusExp(alphaReduce(e1, x, y), alphaReduce(e2, x, y))
	case LamExp(id, t1) if (id==x) => t
	case LamExp(id, t1) if (id!=x) => LamExp(id, alphaReduce(t1, x, y))
    	case _ => t
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
