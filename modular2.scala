// For Parsers
import scala.util.parsing.combinator._
import scala.util.matching.Regex

// For REPL
import scala.tools.jline

trait Nodes {
	abstract class Exp 
	case class Num(n: Int) extends Exp
	case class Add(lhs: Exp, rhs: Exp) extends Exp
	case class Mul(lhs: Exp, rhs: Exp) extends Exp
	case class Id(s: Symbol) extends Exp

	case class App(fun: Exp, arg: Exp) extends Exp
	case class Fun(param: Symbol, body: Exp) extends Exp
}

object ConsoleReader {
  val consoleReader = new jline.console.ConsoleReader()

  def readLine(prompt: String) = consoleReader.readLine(prompt)
}

trait Parser { self: Nodes =>
  def parse(input: String): Exp
  def parseLine(): Exp = parse(ConsoleReader.readLine("> "))
}

// Let * be an operator. If there is no other operator of the same
// precedence, we can process expressions involving * as a list of
// operands, speeding up the parser while avoiding left-recursion
// should * be left-associative.
trait OperandList { self: Nodes =>

  trait OperandList extends RegexParsers {
    // An Associativity maps a constructor and a list of expressions
    // to an AST that conforms to its name
    type Associativity = ((Exp, Exp) => Exp, List[Exp]) => Exp
  
    // Given a list of expressions, construct the following AST tree:
    //
    //           cons ...
    //          /    \
    //        cons  elem3
    //       /    \
    //     cons  elem2
    //    /    \
    // elem0  elem1
    def leftAssoc: Associativity =
      (cons, elements) => elements.tail.foldLeft(elements.head)(cons)
  
    // Mutatis mutandis for right associativity.
    // Pattern-matching is used, for Scala lacks a replacement for
    // Haskell's `foldr1`.
    def rightAssoc: Associativity = (cons, elements) => elements match {
      case List(x) => x
      case x :: xs => cons(x, rightAssoc(cons, xs))
      // Why not generate, on empty elements,
      // an error related to that by `leftAssoc`.
      case _       => Nil.head
    }
  
    // Assemble a parser for the operand list.
    def operandList(
      assoc      : Associativity,
      constructor: (Exp, Exp) => Exp,
      operator   : String,
      operand    : Parser[Exp]
    ) : Parser[Exp] = (
      operand ~ (operator ~ operand).* ^^ {
        case operand ~ others => {
          // The value `otherOperands` is defined solely to give
          // type annotation.
          val otherOperands : List[Exp] = others map (
            (p) => p match { case operator ~ operand => operand }
          )
          assoc(constructor, operand :: otherOperands)
        }
      }
    )
  }
}


/**
 * Parser Implementations
 */
trait FAEParser extends Parser {
	// do i really have to add it here? Parser already has a self type annotation...
	// can those not be propagated?
	self: Nodes =>

	/**
	 * {|x| ((3 + 5) * 7) + x)}(4) = App(Fun('x, Add(...), 4)
	 */
	trait FAEGrammar extends RegexParsers {
		def id: Parser[Exp] = """[a-zA-Z][a-zA-Z0-9_]*""".r ^^ { (n) => Id(Symbol(n)) }
		def num: Parser[Exp] = """[1-9][0-9]*""".r ^^ { (n) => Num(n.toInt) }
		def expr: Parser[Exp] = addExpr
		def addExpr: Parser[Exp] = (
			  mulExpr ~ "+" ~ addExpr ^^ { case lhs ~ "+" ~ rhs => Add(lhs, rhs) }
			| mulExpr
		)
		def mulExpr: Parser[Exp] = (
			  callExpr ~ "*" ~ mulExpr ^^ { case lhs ~ "*" ~ rhs => Mul(lhs, rhs) }
			| callExpr
		)
		// TODO still broken due to left recursion
                //
                // cai 18.11.12
                // `callExpr` doesn't call itself; what we are experiencing may not be
                // left-recursion. Undisputable is the slowness. Parsing the following
                // expression will give a noticeable pause on the bestest of machines;
                // one more nested call shoud overflow Scala's memory before termination.
                // Excessive backtracking suspected.
                //
                //   a(b(c(d(e(f(g))))))
                //
		def callExpr: Parser[Exp] = (
			  atom ~ "(" ~ expr ~ ")" ^^ { case fun ~ "(" ~ args ~ ")" => App(fun, args) }
			| atom
		)
		def atom: Parser[Exp] = (
			  id
			| num
			| "{"~"|"~id~"|" ~ expr ~ "}" ^^ { case "{"~"|"~id~"|" ~ body ~ "}" => id match {
					case Id(s) => Fun(s, body) 
					case _ => scala.sys.error("Formal parameter has to be a symbol")
				}
			}
			| "(" ~ expr ~ ")" ^^ { case "(" ~ e ~ ")" => e }
		)
		def apply(input: String): Exp = parseAll(expr, input) match {
			case Success(result, _) => result
			case failure: NoSuccess => scala.sys.error(failure.msg)
		}		
	}

	// TODO beautify this
	def parse(input: String): Exp = {
		object ParserInstance extends FAEGrammar
		ParserInstance(input)
	}
}


trait LambdaParser extends Parser with OperandList { self: Nodes =>

        val lambda: String = "λ"

	// λx.x
	// "(λx.x)((λx.xx)(λx.xx))" 
	trait LambdaGrammar extends RegexParsers with OperandList {
		def id: Parser[Exp] = """[a-zA-Z]+""".r ^^ { (n) => Id(Symbol(n)) }
		def num: Parser[Exp] = """-?[1-9][0-9]*""".r ^^ { (n) => Num(n.toInt) }
		def expr: Parser[Exp] = addExpr
		def addExpr: Parser[Exp] = operandList(leftAssoc, Add, "+", mulExpr)
		def mulExpr: Parser[Exp] = operandList(leftAssoc, Mul, "*", callExpr)
                // cai 18.11.12
                // `callExpr` is now a list of applications.
                def callExpr: Parser[Exp] = (
                  atom ~ atom.* ^^ { case fun ~ args => leftAssoc(App, fun :: args) }
                )
		def atom: Parser[Exp] = (
			  id
			| num
			| lambda~id~"." ~ expr ^^ { case  lambda~id~"." ~ body => id match {
					case Id(s) => Fun(s, body) 
					case _ => scala.sys.error("Formal parameter has to be a symbol")
				}
			}
			| "(" ~ expr ~ ")" ^^ { case "(" ~ e ~ ")" => e }
		)
		def apply(input: String): Exp = parseAll(expr, input) match {
			case Success(result, _) => result
			case failure: NoSuccess => scala.sys.error(failure.msg)
		}
	}

	def parse(input: String): Exp = {
          object parserObject extends LambdaGrammar
          parserObject(input)
        }
}

trait BackslashParser extends LambdaParser { self: Nodes =>
  override val lambda: String = "\\"
}


trait Interpreter { self: Parser with Nodes =>

	// abstract type for return values
	type T

	def eval(input: Exp): T
	def apply(input: String): T = eval(parse(input))
	def repl(): Unit = while(true) {
		val line = ConsoleReader.readLine("> ")
		if(line == null) {
			println("Ctrl-d")
			return
		}
		if(line == "q")
			return
		
		println(apply(line))	
	}
}

/**
 * Interpreter Implementations
 */

trait EagerInterpreter extends Interpreter { self: Parser with Nodes =>


	sealed abstract class Value
	type Env = Map[Symbol, Value]
	case class NumV(n: Int) extends Value
	case class Closure(lambda: Fun, env: Env) extends Value

	type T = Value

	def evalEnv(input: Exp, env: Env) : Value = input match {
		case Num(n: Int) => NumV(n)
		case Id(s) => env(s)
		case Add(lhs,rhs) => (evalEnv(lhs, env), evalEnv(rhs, env)) match {
		  case (NumV(x), NumV(y)) => NumV(x + y)
		  case _ => sys.error("can only add numbers")
		}
		case Mul(lhs, rhs) => (evalEnv(lhs, env), evalEnv(rhs, env)) match {
			case (NumV(x), NumV(y)) => NumV(x * y)
			case _ => sys.error("can only multiply numbers")
		}
		case f: Fun => Closure(f, env)
		case App(fun, arg) => evalEnv(fun, env) match {
			case Closure(Fun(param, body), closureEnv) => 
				evalEnv(body, closureEnv + (param -> evalEnv(arg, env)))
			case _ => sys.error("can only apply functions")
		}
	}

	def eval(input: Exp): T = evalEnv(input, Map())
}

/**
 * This now works in our Lazy interpreter:
 * (λx.4)((λx.xx)(λx.xx)) 
 *	=> NumV(4)
 */
trait LazyInterpreter extends Interpreter { self: Parser with Nodes =>


	sealed abstract class Value
	sealed abstract class Storable
	// here are the type recursion problems, prof. ostermann told about
	// trying to solve them with inheritance
	type Env = Map[Symbol, Storable]
	case class NumV(n: Int) extends Value
	case class Closure(lambda: Fun, env: Env) extends Value
	case class Thunk(boxedExp: Exp, env: Env) extends Storable

	type T = Value

	def evalEnv(input: Exp, env: Env): Value = input match {
		case Num(n) => NumV(n)
		case Id(s) => env(s) match {
			case Thunk(boxed, defEnv) => evalEnv(boxed, defEnv)
			case _ => sys.error("only thunks, may be evaluated")
		}
		case Add(lhs, rhs) => (evalEnv(lhs, env), evalEnv(rhs, env)) match {
			case (NumV(x), NumV(y)) => NumV(x + y)
			case _ => sys.error("can only add numbers")
		}
		case Mul(lhs, rhs) => (evalEnv(lhs, env), evalEnv(rhs, env)) match {
			case (NumV(x), NumV(y)) => NumV(x * y)
			case _ => sys.error("can only multiply numbers")
		}
		case f: Fun => Closure(f, env)
		case App(fun, arg) => evalEnv(fun, env) match {
		// Use environment stored in closure to realize proper lexical scoping!
			case Closure(Fun(param, body), closureEnv) => 
				evalEnv(body, closureEnv + (param -> Thunk(arg, env)))
	    	case _ => sys.error("can only apply functions")
	    }
	}

	def eval(input: Exp): T = evalEnv(input, Map())
}

/**
 * If0 Extension
 */
trait NodesIf0 extends Nodes {

	implicit def num2exp(n: Int) = Num(n)
	implicit def sym2exp(x: Symbol) = Id(x)

	case class If0(cond : Exp, true_exp : Exp, false_exp : Exp) extends Exp
	case class AppFun(f: Symbol, args: List[Exp]) extends Exp
}


trait BackslashIf0Parser extends BackslashParser {

  self: NodesIf0 =>

  trait If0Grammar extends LambdaGrammar {

    // Not really extensible ...
    override def expr: Parser[Exp] = if0Expr

    // The grammar allows us to write e. g.
    //
    //   ? sunny | happy : ? has_cake | sate : sad
    //
    // with the understanding that each word should be replaced
    // by what may constitute an identifier.
    def if0Expr: Parser[Exp] = (
    	    "?" ~ addExpr ~ "|" ~ addExpr ~ ":" ~ if0Expr ^^ {
              case "?" ~ cond ~ "|" ~ thenExp ~ ":" ~ elseExp
                => If0(cond, thenExp, elseExp)
            }
    	  | addExpr
    )
  }

  override def parse(input: String): Exp = {
    object parserObject extends If0Grammar
    parserObject(input)
  }
}

trait DInterpreter extends EagerInterpreter {
  self: Parser with NodesIf0 =>

  override def evalEnv(input: Exp, env: Env): Value = input match {
    case If0(cond, thenExp, elseExp)
      => evalEnv(cond, env) match {
           case NumV(0) => evalEnv(thenExp, env)
           case _       => evalEnv(elseExp, env)
         }
    case _ => super.evalEnv(input, env)
  }
}


/*

trait FunctionDefinitions { self: NodesIf0 =>
	case class FunDef(args: List[Symbol], body: Exp)
	type Funs = Map[Symbol,FunDef]

	val fib = 'fib -> FunDef('n :: Nil, 
		If0('n, 
			1, 
			If0(Add('n, -1), 
				1, 
				Add(
					AppFun('fib, Add('n, -1) :: Nil), 
					AppFun('fib, Add('n, -2) :: Nil)))))
}
*/
/**
 * Continuation Implementation
 *
trait DynamicInterpreter extends Interpreter { 
	// Dependencies
	self: Parser 
		with NodesIf0 
		with FunctionDefinitions =>

	type T = Int
	type Continuation = (Int, Env) => Int
	
	type Env = Map[Symbol, Int]

	val evaluate_me = 0

	// rewrite to use trampoline
	def evalDynamic(
		e    : Exp,
		funs : Funs = Map(),
		env  : Env = Map(),
		ctn  : Continuation = (result, _) => result
	) : Int = e match {
		// Numbers yield a result; we can call the continuation and do the
		// rest of the computation.
		case Num(n)
		=> ctn(n, env)
		// So are identifiers.
		case Id(x)
		=> ctn(env(x), env)
		// For the sum of two subexpressions,
		// we evaluate the left-hand-side first,
		// giving it as continuation a function that will
		// evaluate the right-hand-side under the environment
		// after evaluation of the left-hand-side, and do the
		// rest of the computation  thereafter.
		case Add(lhs, rhs) => 
		evalDynamic(lhs, funs, env, (value_lhs, env_after_lhs) =>
			evalDynamic(rhs, funs, env_after_lhs, (value_rhs, env_after_rhs) =>
				ctn(value_lhs + value_rhs, env_after_rhs)))

		case Add(lhs, rhs) => 
		evalDynamic(lhs, funs, env, (value_lhs, env_after_lhs) =>
			evalDynamic(rhs, funs, env_after_lhs, (value_rhs, env_after_rhs) =>
				ctn(value_lhs * value_rhs, env_after_rhs)))

		case Mul(lhs, rhs)
			=> evaluate_me
		case With(x, xdef, body)
			=> evaluate_me
		case App(f, args)
			=> evaluate_me
		case If0(cond, true_exp, false_exp) 
			=> evaluate_me
	}

	def eval(input: Exp): T = evalDynamic(input)
}
*/
/**
 * Compare Extension
 */
trait NodesCompare extends Nodes {
	case class Compare(lhs: Exp, rhs: Exp) extends Exp
}

trait FAEParserCompare extends FAEParser {

	self: NodesCompare =>

	trait FAECompareGrammar extends FAEGrammar {

		override def expr: Parser[Exp] = (
			  compExpr
			| super.expr
		)

		def compExpr: Parser[Exp] = (
			  addExpr ~ "==" ~ compExpr ^^ { case lhs ~ "==" ~ rhs => Compare(lhs, rhs) }
			| addExpr
		)
	}
	
	override def parse(input: String): Exp = {
		object ParserInstance extends FAECompareGrammar
		ParserInstance(input)
	}
}

// Works with: ((3 == 5)({|x| 10}))({|x| 20 })
trait InterpreterCompare extends LazyInterpreter { self: Parser with NodesCompare =>

	val True = Closure(Fun('trueBlock, Fun('falseBlock, 
					App(Id('trueBlock), Num(1)))), Map())
	val False = Closure(Fun('trueBlock, Fun('falseBlock, 
					App(Id('falseBlock), Num(0)))), Map())

	override def evalEnv(input: Exp, env: Env): Value = input match {
		case Compare(lhs, rhs) => (evalEnv(lhs, env), evalEnv(rhs, env)) match {
			case (NumV(x), NumV(y)) => if(x == y) True else False
			case _ => sys.error("can only compare numbers")
		}
		case _ => super.evalEnv(input, env)
	}
}
object Compare extends InterpreterCompare with FAEParserCompare with NodesCompare


/**
 * Assembled Interpreters
 */
object LambdaInterpreter extends EagerInterpreter with LambdaParser with Nodes
object FAEInterpreter extends EagerInterpreter with FAEParser with Nodes
object Lazy extends LazyInterpreter with LambdaParser with Nodes

object D extends DInterpreter with BackslashIf0Parser with NodesIf0
