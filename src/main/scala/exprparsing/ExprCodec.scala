package exprparsing

import Models._
import cats.parse.{Parser, Parser0}

object ExprCodec {

  /* parser */

  def epsilonP: Parser0[Unit] = Parser.pure(()) // TODO: check!!!

}
