package audiobooks

final case class BookId private (value: String)
object BookId:
  private val Pattern = "[A-Za-z0-9]+".r
  def make(s: String): Either[ParseError, BookId] =
    val t = s.trim
    Either.cond(Pattern.matches(t), BookId(t), ParseError.InvalidField("bookId", s))

final case class Credits private (value: Int)
object Credits:
  def make(s: String): Either[ParseError, Credits] =
    s.trim.toIntOption.filter(_ > 0).map(Credits(_))
      .toRight(ParseError.InvalidField("credits", s))

enum DomainEvent:
  case MonthlyGrant(amount: Credits)
  case BuyCreditPack(amount: Credits)
  case BuyBook(book: BookId)
  case ReturnBook(book: BookId)

enum ParseError:
  case WrongColumnCount(line: String)
  case UnknownEvent(code: String)
  case InvalidField(field: String, raw: String)

enum DomainError:
  case InsufficientCredits(balance: Int)
  case BookAlreadyOwned(book: BookId)
  case BookNotInLibrary(book: BookId)

final case class State(
                        credits: Int = 0,
                        library: Set[BookId] = Set.empty,
                        returned: Vector[BookId] = Vector.empty
                      ):
  def libraryCount: Int = library.size
  /** Інваріант (ASSUMED): баланс ніколи не від'ємний */
  def invariantHolds: Boolean = credits >= 0