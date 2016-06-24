package hoverboard.term

import hoverboard._
import hoverboard.rewrite.Env
import hoverboard.Name

import scalaz.{Name => _, _}
import Scalaz._

case class Fix(body: Lam,
               index: Name,
               name: Option[String] = None,
               driven: Boolean = false)
  extends Term with FirstOrder[Term] {

  override def drive(env: Env): Term =
    if (driven)
      this
    else {
      val newFix = super.drive(env)
      if (newFix =@= this) this // preserve `name`
      else newFix
    } match {
      case newFix: Fix => newFix.copy(driven = true)
      case other => other
    }

  override def driveHead(env: Env): Term = {
    constantArgs
      .headOption.map(argIdx => removeConstantArg(argIdx).drive(env))
      .getOrElse(this)
  }

  // TODO filter on decreasing/strict args
  override def driveHeadApp(env: Env, args: NonEmptyList[Term]): Term =
    if (args.any(t => t.leftmost.isInstanceOf[Constructor] || t == Bot)) {
      val originalTerm = App(this, args)
      val driven = App(unwrap, args).drive(env)
      val wasProductive = driven.terms.all {
        case term@App(Var(f), xs) if f == body.binding =>
          term.strictlyEmbedsInto(App(Var(f), args))
        case _ =>
          true
      }
      if (wasProductive)
        (driven :/ (this / body.binding)).drive(env.havingSeen(originalTerm))
      else
        super.driveHeadApp(env, args)
    } else {
      super.driveHeadApp(env, args)
    }

  def unfold: Term = body.betaReduce(NonEmptyList(this))

  def unwrap: Term = body.body

  override def mapImmediateSubtermsWithBindings(f: (ISet[Name], Term) => Term): Term = {
    val newBody = f(ISet.empty, body).asInstanceOf[Lam]
    if (newBody =@= body) this
    else Fix(newBody, index)
  }

  override def toString: String =
    name.getOrElse {
      val (bindings, innerBody) = body.flattenLam
      s"fix[${index}] ${bindings.toList.mkString(" ")} -> ${innerBody}"
    }

  override def withName(name: String) = {
    require(freeVars.isEmpty, "Cannot name a fixed-point with free variables")
    copy(name = Some(name))
  }

  def argCount: Int =
    body.flattenLam._1.length - 1  // -1 for the fixed variable

  /**
    * Constant arguments are ones whose value never changes in any recursive function call.
    * {{{
    *   t"fix f x y -> f x (Suc y)".asInstanceOf[Fix].constantArgs == ISet.singleton(0)
    *   t"Add".asInstanceOf[Fix].constantArgs == ISet.singleton(1)
    * }}}
    * @return The indices of any constant arguments to this fixed-point
    */
  def constantArgs: IList[Int] = {
    val (bindings, innerBody) = body.flatten
    require(bindings.toList.size == bindings.toSet.size)
    val fixBinding = bindings.head
    val argBindings = bindings.tail
    val recursiveCalls = innerBody
      .subtermsWithBindings
      .filter { case (bindings, term) =>
        term match {
          case App(Var(f), _) => !bindings.contains(f) && f == fixBinding
          case _ => false
        }
      }

    IList(argBindings.toList.indices : _*).filter { (i: Int) =>
      val arg = argBindings.index(i).get
      recursiveCalls.all {
        case (bindings, App(_, xs)) =>
          !bindings.contains(arg) && xs.index(i).fold(false)(x => x == Var(arg))
        case _ =>
          throw new AssertionError("wat")
      }
    }
  }

  def removeConstantArg(argIdx: Int): Term = {
    val (NonEmptyList(fixBinding, argBindings), innerBody) = body.flatten
    require(argBindings.length > argIdx)
    val (leftArgs, otherArgs) = argBindings.splitAt(argIdx)
    val (removedArg, rightArgs) = (otherArgs.headOption.get, otherArgs.tailOption.get)
    val newInnerBody = innerBody.mapTermsContaining(ISet.singleton(fixBinding)) {
      case App(f, xs) if f == Var(fixBinding) =>
        require(xs.index(argIdx) == Some(Var(removedArg)), "this is not a constant argument")
        f.apply(xs.list.removeAt(argIdx).get)
      case other => other
    }
    val newFixBody = Lam(NonEmptyList.nel(fixBinding, leftArgs ++ rightArgs), newInnerBody)
    val newFix = Fix(newFixBody, index)
    Lam(leftArgs :+ removedArg, newFix.apply(leftArgs.map((x: Name) => Var(x).asInstanceOf[Term])))
  }

  override def arbitraryOrderingNumber: Int = 3

  override def zip(other: Term): Option[IList[(Term, Term)]] =
    other match {
      case other: Fix =>
        Some(IList((body, other.body)))
      case _ =>
        None
    }

  override def order(other: Term) =
    other match {
      case other: Fix =>
        index ?|? other.index |+| body.order(other.body)
      case _ =>
        arbitraryOrderingNumber ?|? other.arbitraryOrderingNumber
    }

  // TODO implement this method for constructors with more or fewer than one recursive argument
  def guessConstructorContext: Option[Context] = {
    val fixArgs = body.flatten._1.tail
    val explored = this.apply(fixArgs.map(n => Var(n): Term)).explore.filter(_ != Bot)
    for {
      potentialContext <- explored.headOption
      constr <- potentialContext.leftmost match {
        case constr: Constructor => Some(constr)
        case _ => None
      }
      if constr.recursiveArgs.size == 1
      recArgIdx = constr.recursiveArgs.toList.head
      context = constr.apply(potentialContext.asInstanceOf[App].args.list.setAt(recArgIdx, Var(contextGap)))
      if context.freeVars.delete(contextGap).isSubsetOf(this.freeVars)
      if explored.all(t => context.unifyLeft(t).exists(_.boundVars == ISet.singleton(contextGap)))
    } yield context
  }

  def fissionConstructorContext: Option[Term] = {
    require(!freeVars.contains(contextGap), s"Don't use $contextGap as a variable name, it's used for context gaps.")
    for {
      ctx <- guessConstructorContext
      fixArgs = body.flatten._1.tail
      expandedCtx = Lam(fixArgs, ctx :/ (Var(body.binding).apply(fixArgs.map(n => Var.apply(n): Term)) / contextGap))
      driven = body.apply(expandedCtx).drive
      (fixArgs2, drivenBody) = driven.flattenLam
      stripped <- drivenBody
        .stripContext(ctx)
        .tap(_ => assert(fixArgs == fixArgs2))
    } yield Fix(Lam(body.binding, Lam(fixArgs, stripped)), index)
  }

  lazy val strictArgs: IList[Int] = {
    val vars = body.body.flattenLam._1.map(x => Var(x): Term)
    IList(0.until(argCount): _*).filter { i =>
      val args = vars.setAt(i, Bot)
      this.apply(args).drive == Bot
    }
  }

  override protected def getIndices = super.getIndices.insert(index)
}
