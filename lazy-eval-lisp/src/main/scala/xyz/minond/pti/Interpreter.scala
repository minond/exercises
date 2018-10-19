package xyz.minond.pti

object Interpreter {
  object Message {
    def GIVEN(thing: String) =
      s"Given $thing."

    def ERR_SYNTAX(err: Parser.Error) =
      s"Syntax error:\n${err.stringify("  ")}"
    def ERR_BAD_SYNTAX(thing: String) =
      s"${thing}: bad syntax"

    def ERR_NO_ASSIGNMENT(fn: String) =
      s"Assignment disallowed with $fn"
    val ERR_REC_LOOKUP =
      "Detected recursive lookup. Stopping evaluation."
    def ERR_UNDEFINED_LOOKUP(label: String) =
      s"${label} is undefined."
    def ERR_ARITY_MISMATCH(expected: Int, got: Int) =
      s"Arity mismatch, expected ${expected} arguments but got ${got}."
    def ERR_BAD_ARGS(fn: String, args: String*) =
      s"Bad arguments passed to $fn proc, expecting ${args.mkString(", ")}."
    def ERR_INVALID_ERROR(exprs: List[Expression]) =
      s"Cannot use `(${exprs.mkString(" ")})` as an error message."

    val ERR_INTERNAL =
      "Internal error"
    def ERR_EVAL_EXPR(expr: Expression) =
      s"Error evaluating $expr."

    val ERR_PROC_EMPTY_CALL =
      "Missing procedure expression"
    val ERR_PROC_NON_PROC_CALL =
      "Call made to non-procedure."
    val ERR_PROC_NON_ID_ARG =
      "Found non-identifier argument(s) in lambda expression."
    def ERR_PROC_DUP_ARGS(dups: List[String]) =
      s"Found duplicate argument name(s): ${dups.mkString(", ")}."
  }

  def eval(code: String, env: Environment): (List[Expression], Environment) =
    eval(new Parser(new Tokenizer(code)).toList, env)

  def eval(expr: Expression, env: Environment): Expression =
    eval(Right(expr), env)._1

  def eval(exprs: List[Expression], env: Environment): List[Expression] =
    exprs.map(eval(_, env))

  def eval(
      exprs: List[Expression],
      env: Environment,
      pf: PartialFunction[Expression, Expression])
    : (List[Expression], List[Expression]) = {
    val vals = eval(exprs, env)
    (vals, vals.collect(pf))
  }

  def eval(
      exprs: List[Either[Parser.Error, Expression]],
      env: Environment): (List[Expression], Environment) =
    exprs.foldLeft[(List[Expression], Environment)]((List.empty, env)) {
      case ((ret, env), expr) =>
        val (value, next) = eval(expr, env)
        (ret :+ value, next)
    }

  def eval(
      expr: Either[Parser.Error, Expression],
      env: Environment): (Expression, Environment) = {
    expr match {
      case Right(Bool(value)) => (Bool(value), env)
      case Right(Integer(value)) => (Integer(value), env)
      case Right(Real(value)) => (Real(value), env)
      case Right(Str(value)) => (Str(value), env)
      case Right(Quote(Identifier(name), _)) => (Quote(Identifier(name)), env)
      case Right(Quote(SExpr(xs), _)) => (Quote(SExpr(xs)), env)
      case Right(Quote(Quote(xs, _), _)) => (Quote(Quote(xs)), env)
      case Right(Quote(value, _)) => (value.unQuote, env)
      case Right(Identifier(name)) => (lookup(name, env), env)
      case Right(SExpr(fn :: args)) => procCall(fn, args, env)
      case Right(SExpr(Nil)) => (Error(Message.ERR_PROC_EMPTY_CALL), env)
      case Right(Dict(vals)) => (Dict(vals), env)
      case Right(Pair(a, b)) => (Pair(a, b), env)
      case Right(Proc(args, body, env, del)) => (Proc(args, body, env, del), env)
      case Right(Procedure(fn)) => (Procedure(fn), env)
      case Right(Error(msg, prev)) => (Error(msg, prev), env)
      case Left(err) => (Error(Message.ERR_SYNTAX(err)), env)
    }
  }

  def safe(work: => Expression): Expression = {
    try {
      work
    } catch {
      case _: StackOverflowError => Error(Message.ERR_REC_LOOKUP)
    }
  }

  def procDef(
      rawArgs: List[Expression],
      body: Expression,
      env: Environment,
      delayed: Boolean = false): Expression = {
    val (args, errs) = rawArgs.partition {
      case Identifier(_) => true
      case _ => false
    }

    val names = args.map {
      case Identifier(name) => name
      case _ => ""
    }

    val dups = names.groupBy(identity) collect {
      case (x, xs) if xs.size > 1 => x
    }

    if (dups.size > 0) Error(Message.ERR_PROC_DUP_ARGS(dups.toList))
    else if (errs.size > 0) Error(Message.ERR_PROC_NON_ID_ARG)
    else Proc(names, body, env, delayed)
  }

  def lookup(name: String, env: Environment): Expression =
    safe(env.lookup(name).getOrElse(Error(Message.ERR_UNDEFINED_LOOKUP(name))))

  def procCall(
      fn: Expression,
      args: List[Expression],
      env: Environment): (Expression, Environment) =
    eval(fn, env) match {
      case proc: Proc =>
        if (!proc.validArity(args.size))
          (Error(Message.ERR_ARITY_MISMATCH(proc.args.size, args.size)), env)
        else if (proc.delayed)
          (safe(eval(proc.body, proc.scope(args, proc.env, env))), env)
        else {
          eval(args, env, {
            case err: Error => err
          }) match {
            case (values, Nil) =>
              (safe(eval(proc.body, proc.scope(values, proc.env, env))), env)
            case (_, err :: _) => (err, env)
          }
        }

      case Procedure(fn) => fn(args, env)
      case err: Error => (err, env)

      case _ =>
        (
          Error(Message.ERR_PROC_NON_PROC_CALL, Some(Error(Message.GIVEN(fn.toString)))),
          env)
    }

  def ok(info: QuoteInfo = UserSpace): Expression =
    Quote(Identifier("ok"), info)
}
