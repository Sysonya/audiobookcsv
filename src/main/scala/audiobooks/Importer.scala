package audiobooks

import DomainEvent.*

object Importer:
  private val BookCost = 1 // ASSUMED

  def apply(s: State, e: DomainEvent): Either[DomainError, State] = e match
    case MonthlyGrant(a)  => Right(s.copy(credits = s.credits + a.value))
    case BuyCreditPack(a) => Right(s.copy(credits = s.credits + a.value))

    case BuyBook(b) =>
      if s.credits < BookCost then Left(DomainError.InsufficientCredits(s.credits))
      else if s.library.contains(b) then Left(DomainError.BookAlreadyOwned(b))
      else Right(s.copy(credits = s.credits - BookCost, library = s.library + b))

    case ReturnBook(b) =>
      if !s.library.contains(b) then Left(DomainError.BookNotInLibrary(b))
      else Right(s.copy(
        credits  = s.credits + BookCost,
        library  = s.library - b,
        returned = s.returned :+ b))

  final case class Report(state: State, rejected: List[(DomainEvent, DomainError)])

  def fold(events: Iterable[DomainEvent]): Report =
    val (st, rej) = events.foldLeft((State(), List.empty[(DomainEvent, DomainError)])) {
      case ((s, r), e) => apply(s, e) match
        case Right(n)  => (n, r)
        case Left(err) => (s, (e, err) :: r)
    }
    Report(st, rej.reverse)

  private val Header = "event,p1,p2,p3,p4"

  def importFile(path: String): (List[(String, ParseError)], Report) =
    val src = scala.io.Source.fromFile(path, "UTF-8")
    try
      val lines = src.getLines().filter(l => l.trim.nonEmpty && l.trim != Header).toList
      val (bad, good) = Parser.parseAll(lines)
      (bad, fold(good))
    finally src.close()