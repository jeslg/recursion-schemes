package dev.habla

enum Expr:

  case Lit(a: Int)
  case Ident(v: String)
  case Add(x: Expr, y: Expr)
  case Mul(x: Expr, y: Expr)

  def eval(env: Map[String, Int]): Int = this match {
    case Lit(i) => i
    case Ident(s) => env(s)
    case Add(x, y) => x.eval(env) + y.eval(env)
    case Mul(x, y) => x.eval(env) * y.eval(env)
  }

  def pretty: String = this match {
    case Lit(i) => i.toString
    case Ident(s) => s
    case Add(x, y) => s"(${x.pretty} + ${y.pretty})"
    case Mul(x, y) => s"(${x.pretty} * ${y.pretty})"
  }

  def mul0: Expr = optim(_ match {
    case Mul(Lit(0), _) | Mul(_, Lit(0)) => Lit(0)
    case e => e
  })

  def add0: Expr = optim(_ match {
    case Add(Lit(0), y) => y
    case Add(x, Lit(0)) => x
    case e => e
  })

  def optim(f: Expr => Expr): Expr = this match {
    case Lit(i) => Lit(i)
    case Ident(s) => Ident(s)
    case Add(x, y) => Add(f(x.optim(f)), f(y.optim(f)))
    case Mul(x, y) => Mul(f(x.optim(f)), f(y.optim(f)))
  }

  override def toString: String = pretty

object Motivation:
  import Expr._

  def (x: Expr) + (y: Expr): Expr = Add(x, y)
  def (x: Expr) * (y: Expr): Expr = Mul(x, y)

  val expr = Lit(1) + Lit(2) + (Lit(33) * Lit(0)) + Ident("x") * Lit(0)
  val expr2 = expr.mul0
  val expr3 = expr2.add0

trait Functor[F[_]]:
  def [A, B] (fa: F[A]) map (f: A => B): F[B]

enum ExprF[A]:

  case Lit(a: Int)
  case Ident(v: String)
  case Add(x: A, y: A)
  case Mul(x: A, y: A)

import ExprF._

// this should be derived automatically
given Functor[ExprF]:
  def [A, B] (fa: ExprF[A]) map (f: A => B): ExprF[B] = fa match {
    case Lit(i) => Lit(i)
    case Ident(s) => Ident(s)
    case Add(x, y) => Add(f(x), f(y))
    case Mul(x, y) => Mul(f(x), f(y))
  }

object Motivation2:

  val expr: ExprF[Int] = Lit(1)
  val expr2: ExprF[ExprF[Int]] = Add(Lit(1), Lit(2))
  val expr3: ExprF[ExprF[ExprF[Int]]] = Add(Add(Lit(1), Lit(0)), Lit(0))

case class Fix[F[_]](unfix: F[Fix[F]])

type Expr2 = Fix[ExprF]

object Motivation3:

  def (x: Expr2) + (y: Expr2): Expr2 = Fix(Add(x, y))
  def (x: Expr2) * (y: Expr2): Expr2 = Fix(Mul(x, y))
  def lit(i: Int): Expr2 = Fix(Lit(i))
  def ident(s: String): Expr2 = Fix(Ident(s))
 
  val expr: Expr2 = lit(1)
  val expr2: Expr2 = lit(1) + lit(2)
  val expr3: Expr2 = lit(1) + lit(0) + lit(0)
  val expr4 = lit(1) + lit(2) + (lit(33) * lit(0)) + ident("x") * lit(0)

type Algebra[F[_], A] = F[A] => A

object Catamorphism:

  def cata[F[_]: Functor, A](alg: Algebra[F, A])(fx: Fix[F]): A =
    alg(fx.unfix.map(cata(alg)))
  
  import Motivation3._

  def (e: Expr2) optim (f: Expr2 => Expr2): Expr2 =
    cata[ExprF, Expr2](fa => f(Fix(fa)))(e)

  def (e: Expr2) add0: Expr2 = e.optim {
    case Fix(Add(Fix(Lit(0)), y)) => y
    case Fix(Add(x, Fix(Lit(0)))) => x
    case e => e
  }

  def (e: Expr2) mul0: Expr2 = e.optim {
    case Fix(Mul(Fix(Lit(0)), _) | Mul(_, Fix(Lit(0)))) => lit(0)
    case e => e
  }

  def (e: Expr2) pretty: String =
    cata[ExprF, String](_ match {
      case Lit(i) => i.toString
      case Ident(s) => s
      case Add(x, y) => s"$x + $y"
      case Mul(x, y) => s"$x * $y"
    })(e)

