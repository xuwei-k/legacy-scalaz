package scalaz
package parse
package base

sealed trait FailureReason
case object NoParse extends FailureReason
case object MissingOpeningBracket extends FailureReason
case object MissingClosingBracket extends FailureReason
case object UnexpectedEOF extends FailureReason
case object LexicalError extends FailureReason
case class UnexpectedInput[T](t: T) extends FailureReason
case object Failed extends FailureReason
case object NoSuppliedParserSucceeded extends FailureReason
case class MoreItemsExpected(n: Int) extends FailureReason
case class CouldNotParseAnyOf(choices: List[FailureReason]) extends FailureReason

case class FailureWithContext[P, T](token: T, reason: FailureReason) extends FailureReason
