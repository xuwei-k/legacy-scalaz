package scalaz
package parse

package object huttonmeijerwallace {
  type ParseResult[S, T, E, A] = Either[E, List[(A, S, List[Either[E, T]])]]
}