package elea.term

import elea.LispPrintSettings
import elea.rewrite.Env

import scalaz.NonEmptyList

case object Bot extends Atom {
  override def reduceHeadApp(env: Env, args: NonEmptyList[Term]): Term = this

  override def reduceHeadCase(env: Env, enclosingCase: Case): Term = this

  override def arbitraryOrderingNumber: Int = 1

  override def toLisp(settings: LispPrintSettings): String = "⊥"
}
