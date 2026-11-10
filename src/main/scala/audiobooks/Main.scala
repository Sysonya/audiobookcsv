package audiobooks

@main def run(): Unit =
  val (bad, rep) = Importer.importFile("src/main/resources/variant_31_audiobook_credits.csv")
  val s = rep.state
  println(s"Доступні кредити:         ${s.credits}")
  println(s"Книг у бібліотеці:        ${s.libraryCount}")
  println(s"Повернених книг:          ${s.returned.size}")
  println(s"Інваріант (баланс >= 0):  ${s.invariantHolds}")
  println(s"Невалідних рядків (Left): ${bad.size}")
  println(s"Відхилено доменом:        ${rep.rejected.size}")
  println("Історія повернень (перші 10): " + s.returned.take(10).map(_.value).mkString(", "))
  println("Приклади помилок парсингу:")
  bad.take(5).foreach(println)