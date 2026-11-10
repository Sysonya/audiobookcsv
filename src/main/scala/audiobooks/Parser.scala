package audiobooks

import DomainEvent.*

object Codes:
  val MonthlyGrant  = "MONTHLY_GRANT"
  val BuyCreditPack = "BUY_CREDIT_PACK"
  val BuyBook       = "BUY_BOOK"
  val ReturnBook    = "RETURN_BOOK"

object Parser:
  def parseLine(line: String): Either[ParseError, DomainEvent] =
    val cols = line.split(",", -1).map(_.trim)
    if cols.length != 5 then Left(ParseError.WrongColumnCount(line))
    else
      val Array(ev, p1, _, _, _) = cols
      ev match
        case Codes.MonthlyGrant  => Credits.make(p1).map(MonthlyGrant(_))
        case Codes.BuyCreditPack => Credits.make(p1).map(BuyCreditPack(_))
        case Codes.BuyBook       => BookId.make(p1).map(BuyBook(_))
        case Codes.ReturnBook    => BookId.make(p1).map(ReturnBook(_))
        case other               => Left(ParseError.UnknownEvent(other))

  def parseAll(lines: Iterable[String]): (List[(String, ParseError)], List[DomainEvent]) =
    lines.toList.partitionMap(l => parseLine(l).left.map(e => (l, e)))