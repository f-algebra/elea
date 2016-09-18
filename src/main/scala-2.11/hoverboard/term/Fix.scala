package hoverboard.term

import hoverboard._
import hoverboard.rewrite.Env
import hoverboard.Name

import scalaz.Ordering.{LT, GT, EQ}
import scalaz.{Name => _, _}
import Scalaz._

case class Fix(body: Term,
               index: Fix.Index,
               name: Option[String] = None,
               driven: Boolean = false)
  extends Term with FirstOrder[Term] {

  require(name.isEmpty || freeVars.isEmpty,
    s"""Cannot name a fixed point with free variables. Name "${name.get}", free variables "$freeVars"""")

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
  override def driveHeadApp(env: Env, args: NonEmptyList[Term]): Term = {
    strictArgIndices.find(args.index(_).get.isInstanceOf[Case]) match {
      case Some(caseIdx) =>
        // If a pattern match is a strict argument to a fixed-point,
        // we can float it out to be topmost
        val caseArg = args.index(caseIdx).get.asInstanceOf[Case]
        C(x => this.apply(args.list.setAt(caseIdx, Var(x))))
          .applyToBranches(caseArg)
          .driveIgnoringMatchedTerm(env)
      case None =>
        body match {
          case body: Lam if args.any(t => t.leftmost.isInstanceOf[Constructor] || t == ⊥) =>
            // If an argument to a fixed-point is a constructor or a ⊥, we can try to unfold
            // the fixed-point
            val originalTerm = App(this, args)
            val driven = App(body.body, args).drive(env)
            lazy val wasProductive = driven.subtermsContaining(ISet.singleton(body.binding)).all {
              case term@App(Var(f), xs) if f == body.binding =>
                term.strictlyEmbedsInto(App(Var(f), args))
              case _ =>
                true
            }
            if (!driven.isInstanceOf[Case] && wasProductive)
              (driven :/ (this / body.binding)).drive(env.havingSeen(originalTerm))
            else
              super.driveHeadApp(env, args)
          case _ =>
            super.driveHeadApp(env, args)
        }
    }
  }

  override def unfold: Term = body.betaReduce(NonEmptyList(this))

  override def mapImmediateSubtermsWithBindings(f: (ISet[Name], Term) => Term): Term = {
    val newBody = f(ISet.empty, body)
    if (newBody =@= body) this
    else Fix(newBody, index)
  }

  override def toString: String =
    name.map(n => Name.asDefinition(n) + index.toString).getOrElse {
      val (bindings, innerBody) = body.flattenLam
      s"fix$index ${bindings.toList.mkString(" ")} -> $innerBody"
    }

  override def withName(name: String) =
    copy(name = Some(name))

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
  def constantArgs: IList[Int] =
    body match {
      case body: Lam =>
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
      case _ =>
        IList.empty
    }

  def removeConstantArg(argIdx: Int): Term =
    body match {
      case body: Lam =>
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
      case _ =>
        throw new AssertionError("Cannot remove constant arguments from fixed-points with non-lambda bodies")
  }

  override def arbitraryOrderingNumber: Int = 3

  override def zip(other: Term): Option[IList[(Term, Term)]] =
    other match {
      case other: Fix if index.isOmega && other.index.isOmega || index == other.index =>
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
  def guessConstructorContext: Option[Context] =
    body match {
      case body: Lam =>
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
          context = C(ctxGap => constr.apply(potentialContext.asInstanceOf[App].args.list.setAt(recArgIdx, Var(ctxGap))))
          if context.freeVars.isSubsetOf(this.freeVars)
          if explored.all(t => context.strip(t).isDefined)
        } yield context
      case _ =>
        None
    }

  def fissionConstructorContext: Option[(Context, Fix)] =
    body match {
      case body: Lam =>
        for {
          ctx <- guessConstructorContext
          fixArgs = body.flatten._1.tail
          expandedCtx = C(gap => Lam(fixArgs, ctx.apply(Var(gap).apply(fixArgs.map(n => Var.apply(n): Term)))))
          driven = body.apply(expandedCtx.apply(Var(body.binding))).drive
          (fixArgs2, drivenBody) = driven.flattenLam
          stripped <- ctx
            .strip(drivenBody)
            .tap(_ => assert(fixArgs == fixArgs2))
        } yield (ctx, Fix(Lam(body.binding, Lam(fixArgs, stripped)), index))
      case _ =>
        None
    }

  lazy val strictArgIndices: IList[Int] =
    body match {
      case body: Lam =>
        val vars = body.body.flattenLam._1.map(x => Var(x): Term)
        IList(0.until(argCount): _*).filter { i =>
          val args = vars.setAt(i, Bot)
          body.body.apply(args).drive == Bot
        }
      case _ =>
        IList.empty
    }

  final def strictArgs(args: IList[Term]): IList[Term] = {
    var strict = IList.empty[Term]
    strictArgIndices.toList.foreach { i =>
      args.index(i).foreach { t => strict = t :: strict }
    }
    strict
  }

  override lazy val indices = super.indices.insert(index)

  /**
    * Is fixed-point promoted form
    */
  def isFPPF(args: IList[Term]): Boolean =
    args.all(_.isInstanceOf[Var]) &&
      args.distinct == args &&
      freeVars.intersection(ISet.unions(args.map(_.freeVars).toList)).isEmpty

  override def freshenIndices: Fix = copy(index = index.freshen)
}

object Fix {
  case class Index private(name: Name, isFinite: Boolean) {
    override def toString =
      if (isFinite) s"[$name]" else ""

    def asFinite: Index = copy(isFinite = true)
    def asOmega: Index = copy(isFinite = false)
    def freshen: Index = copy(name = Name.fresh("α"))
    def isOmega: Boolean = !isFinite
  }

  def emptyIndex: Index = omega(Name("empty"))
  def finite(name: Name): Index = Index(name, true)
  def omega(name: Name): Index = Index(name, false)

  def freshOmegaIndex: Index = emptyIndex.freshen.asOmega
  def freshFiniteIndex: Index = emptyIndex.freshen.asFinite

  implicit val fixIndexOrder: Order[Fix.Index] = (x: Index, y: Index) =>
    x.isFinite ?|? y.isFinite |+| x.name ?|? y.name
}
