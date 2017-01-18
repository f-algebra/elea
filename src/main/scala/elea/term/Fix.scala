package elea.term

import elea.Name
import elea.rewrite.Env

import scalaz.Ordering.{EQ, GT, LT}
import scalaz.{Name => _, _}
import Scalaz._

case class Fix(body: Term,
               index: Fix.Index,
               name: Option[String] = None)
  extends Term with FirstOrder[Term] {

  override def reduce(env: Env): Term = {
    val newFix = super.reduce(env)
    if (newFix =@= this) this // preserve `name`
    else newFix
  }

  override def reduceHead(env: Env): Term = {
    constantArgs
      .headOption.map(argIdx => removeConstantArg(argIdx).reduce(env))
      .getOrElse {
        body match {
          case body: Lam if !body.body.freeVars.contains(body.binding) =>
            body.body
          case body: Bot.type =>
            Bot
          case _ =>
            this
        }
      }
  }

  // TODO filter on decreasing/strict args
  override def reduceHeadApp(env: Env, args: NonEmptyList[Term]): Term = {
    if (strictArgs(args.list).any(_ == Bot)) {
      Bot
    } else {
      strictArgIndices.find(i => i < args.size && args.index(i).get.isInstanceOf[Case]) match {
        case Some(idx) =>
          args.index(idx).get match {
            // If a pattern match is a strict argument to a fixed-point,
            // we can float it out to be topmost
            case caseArg: Case =>
              C(x => this.apply(args.list.setAt(idx, Var(x))))
                .applyToBranches(caseArg)
                .reduceIgnoringMatchedTerm(env)
          }
        case None =>
          body match {
            case body: Lam if args.any(t => t.leftmost.isInstanceOf[Constructor] || t == ⊥) =>
              // If an argument to a fixed-point is a constructor or a ⊥, we can try to unfold
              // the fixed-point
              val originalTerm = App(this, args)
              val reduced = App(body.body, args).reduce(env)

              val recursiveSubterms = reduced.subtermsContaining(ISet.singleton(body.binding))

              // TODO try all recursive subterms which are pattern matches must match over a sub-term, instead of the general is case-of logic

              lazy val wasProductive = recursiveSubterms.all {
                case term@App(Var(f), xs) if f == body.binding =>
                  term.strictlyEmbedsInto(App(Var(f), args))
                case _ =>
                  true
              }

              // TODO what about examples that remove all recursive subterms after multiple unfoldings?

              // Remember the ".lteq (.count n xs) (.count n (.app xs ys))" example next time you feel
              // like simplifying this unfolding logic
              if (recursiveSubterms.isEmpty || (!reduced.isInstanceOf[Case] && wasProductive))
                (reduced :/ (this / body.binding)).reduce(env.havingSeen(originalTerm))
              else
                super.reduceHeadApp(env, args)
            case _ =>
              super.reduceHeadApp(env, args)
          }
      }
    }
  }

  override def unfold: Term = body.betaReduce(NonEmptyList(this))

  override def mapImmediateSubtermsWithBindings(f: (ISet[Name], Term) => Term): Term = {
    val newBody = f(ISet.empty, body)
    if (newBody =@= body)
      // Preserve things like `name` if nothing semantically has changed
      copy(body = newBody)
    else
      Fix(newBody, index)
  }

  override def toString: String =
    name.map(n => Name.asDefinition(n) + index.toString).getOrElse {
      val (bindings, innerBody) = body.flattenLam
      s"fix$index ${bindings.toList.mkString(" ")} -> $innerBody"
    }

  override def withName(name: String) =
    copy(name = Some(name))

  lazy val isProductive: Boolean = {
    val (bindings, innerBody) = body.flattenLam
    bindings match {
      case INil() =>
        false
      case ICons(fixVar, _) =>
        def productiveBranch(branch: Branch): Boolean =
          branch.immediateSubtermsWithBindings.all {
            case (branchVars, branchTerm) =>
              !branchVars.contains(fixVar) && productiveTerm(branchTerm)
          }

        def productiveTerm(term: Term): Boolean =
          term match {
            case _ if !term.freeVars.contains(fixVar) =>
              true
            case term: Case =>
              !term.matchedTerm.freeVars.contains(fixVar) &&
                term.branches.all(productiveBranch)
            case _ =>
              term.leftmost.isInstanceOf[Constructor]
          }

        productiveTerm(innerBody)
    }
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
          context <- constr.recursiveArgs.toList match {
            case Seq() =>
              Some(C(_ => potentialContext))
            case Seq(recArgIdx) =>
              Some(C(ctxGap => constr.apply(potentialContext.asInstanceOf[App].args.list.setAt(recArgIdx, Var(ctxGap)))))
            case _ =>
              // TODO implement the case for recursive argument count > 1
              None
          }
          if context.freeVars.isSubsetOf(this.freeVars)
          if explored.all(t => context.strip(t).isDefined)
        } yield context
      case _ =>
        None
    }

  final def fissionConstructorContext(args: IList[Term]): Option[Term] =
    fissionConstructorContext.map {
      case (context, newFix) => context.apply(newFix.apply(args))
    }

  lazy val fissionConstructorContext: Option[(Context, Fix)] =
    body match {
      case body: Lam =>
        for {
          ctx <- guessConstructorContext
          fixArgs = body.flatten._1.tail
          expandedCtx = C(gap => Lam(fixArgs, ctx.apply(Var(gap).apply(fixArgs.map(n => Var.apply(n): Term)))))
          reduced = body.apply(expandedCtx.apply(Var(body.binding))).reduce
          (fixArgs2, reducedBody) = reduced.flattenLam
          stripped <- ctx
            .strip(reducedBody)
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
          body.body.apply(args).reduce == Bot
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

  /**
    * Is fixed-point promoted form
    */
  def isFPPF(args: IList[Term]): Boolean =
    args.all(_.isInstanceOf[Var]) &&
      args.distinct == args &&
      freeVars.intersection(ISet.unions(args.map(_.freeVars).toList)).isEmpty
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

  implicit val fixIndexOrder: Order[Fix.Index] = new Order[Fix.Index] {
    override def order(x: Index, y: Index): Ordering =
      x.isFinite ?|? y.isFinite |+| x.name ?|? y.name
  }
}
