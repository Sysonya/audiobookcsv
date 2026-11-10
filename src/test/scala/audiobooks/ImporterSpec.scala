package audiobooks

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class ImporterSpec extends AnyFunSuite with Matchers:
  private def run(lines: String*) =
    val (bad, good) = Parser.parseAll(lines)
    (bad, Importer.fold(good))

  test("короткий сценарій: нарахування, купівля, повернення, пакет") {
    val (bad, rep) = run(
      "MONTHLY_GRANT,3,,,",
      "BUY_BOOK,B1,,,",
      "RETURN_BOOK,B1,,,",
      "BUY_CREDIT_PACK,10,,,")
    bad shouldBe empty
    rep.rejected shouldBe empty
    rep.state.credits shouldBe 13
    rep.state.libraryCount shouldBe 0
    rep.state.returned.map(_.value) shouldBe Vector("B1")
    rep.state.invariantHolds shouldBe true
  }

  test("фрагмент реального CSV") {
    val (bad, rep) = run(
      "BUY_BOOK,3CWHN27,,,",
      "BUY_CREDIT_PACK,2,,,",
      "MONTHLY_GRANT,3,,,",
      "BUY_BOOK,QLK2LQKE14,,,",
      "BUY_BOOK,64XLFK1JR,,,",
      "BUY_CREDIT_PACK,7,,,",
      "RETURN_BOOK,64XLFK1JR,,,")
    bad shouldBe empty
    rep.rejected should have size 1 // перша BUY_BOOK при балансі 0
    rep.state.libraryCount shouldBe 1
    rep.state.credits shouldBe 11
    rep.state.invariantHolds shouldBe true
  }

  test("баланс 0 забороняє взяття книги") {
    val (_, rep) = run("BUY_BOOK,B1,,,")
    rep.rejected.map(_._2) shouldBe List(DomainError.InsufficientCredits(0))
    rep.state.libraryCount shouldBe 0
  }

  test("повернення книги, якої немає в бібліотеці, відхиляється") {
    val (_, rep) = run("MONTHLY_GRANT,1,,,", "RETURN_BOOK,X1,,,")
    rep.rejected should have size 1
    rep.state.credits shouldBe 1
  }

  test("невалідні рядки потрапляють у Left") {
    val (bad, good) = Parser.parseAll(List("garbage,1,,,", "MONTHLY_GRANT,-5,,,", "MONTHLY_GRANT,1"))
    bad should have size 3
    good shouldBe empty
  }

  test("повний CSV: інваріант тримається, зіпсованих рядків небагато") {
    val (bad, rep) = Importer.importFile("src/main/resources/variant_31_audiobook_credits.csv")
    rep.state.invariantHolds shouldBe true
    bad.size should be < 50000 // ~5% від 500 000
  }