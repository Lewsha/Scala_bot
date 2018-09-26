package Bot

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/*
* Класс для парсинга команд, приходящих из Телеграмма
* */
class CommandParser extends RegexParsers {
  override protected val whiteSpace: Regex = """ +""".r // Создаем спецрегулярку под пробел
  //(как сообщили мне знающие люди, пробел в джавке немного кривой)
  //
  // Далее методы для разбора различных команд

  //  /create_poll <название_опроса:string> [анонимность:yes|no]
  //[видимость результата:afterstop|continuous] [время начала:date] [время окончания:date]
  /*Пояснения:
        - участвовать в опросе может любой пользователь, но управлять
        им - только создавший
        - анонимность заключается в том, что не сохраняются ответы
        пользователей (только факт ответа и результат)
        - видимость результатов "afterstop" - результаты опроса можно
        посмотреть только после его окончания, "continuous" - в процессе
        - если время начала опроса задано, то бот автоматически
        стартует опрос в это время и вручную его стартовать нельзя
        - если время конца опроса задано, то бот автоматически
        завершает опрос в это время и вручную его завершить нельзя
        - участвовать можно только в активных опросах (опросы которые
        были начаты, но еще не завершены)
        - после старта опроса изменять его нельзя!
        */
  def createPoll: Parser[Command] = {
    // [что-то] ~> [что-то] <~ [что-то] - мы ищем всю подстроку (в данном случае начинающуюся и
    // заканчивающуюся скобками, но в результат мы берем только то, что между ~> и <~
    val pollName = Parser("(" ~> """[_a-zA-Zа-яА-ЯёЁ0-9.,:;'"*&!? ]+""".r <~ ")") // имя пула вопросов
    val anonymity = Parser("(" ~> ("yes" | "no") <~ ")")  // анонимность
    val continuous = Parser("(" ~> ("afterstop" | "continuous") <~ ")")  // видимость результата
    val startTime = Parser("(" ~> """\d{2}:\d{2}:\d{2} \d{2}:\d{2}:\d{2}""".r <~ ")")  // время,
    // с которого можно отвечать
    val stopTime = startTime  // заглушка

    "/create_poll" ~> pollName ~ anonymity.? ~ continuous.? ~ startTime.? ~ stopTime.? ^^
      /*
      .? - переменная может отсутствовать
      ~ - что-то вроде задания порядка выполнения
      (сначала один парсер отработает, затем другой отработает на той строке, что останется после выполнения первого),
      ^^ - накинуть обработчик (например, превратить кусок строки сразу в дату */
      /*case - синтаксический сахар (засунуть результаты в переменные и передать в Матчер)
      case - pattern matching
      */
      { case name ~ anon ~ cont ~ start ~ stop => Matcher.CreatePollM(name, anon, cont, start, stop) }
  }

  //  /list - Список голосований
  //Ответ бота - Список опросов с идентификаторами (как минимум)
  def list: Parser[Command] = """^/list""".r ^^ { _ => Matcher.ListM() }

  //  /delete_poll <идентификатор опроса:digits> - Удаление опроса
  //Ответ бота - сообщение об успехе или ошибка
  def deletePoll: Parser[Command] =
    "/delete_poll" ~> "(" ~>"""\d+""".r <~ ")" ^^ { d => Matcher.DeletePollM(d.toInt) }

  //  /start_poll <идентификатор опроса:digits> - Старт процесса голосования
  //Ответ бота - сообщение об успехе или ошибка
  def startPool: Parser[Command] = "/start_poll" ~> "(" ~> """\d+""".r <~ ")" ^^ { d => Matcher.StartPollM(d.toInt) }

  //  /stop_poll <идентификатор опроса:digits> - Стоп процесса голосования
  //Ответ бота - сообщение об успехе или ошибка
  def stopPoll: Parser[Command] = "/stop_poll" ~> "(" ~> """\d+""".r <~ ")" ^^ { d => Matcher.StopPollM(d.toInt) }

  //  /result <идентификатор опроса:digits> - Посмотреть результаты голосования
  //Ответ бота - Красивый отчет о текущем/прошедшем голосовании
  def result: Parser[Command] = "/result" ~> "(" ~> """\d+""".r <~ ")" ^^ { d => Matcher.ResultM(d.toInt) }

  //  /begin <идентификатор опроса:digits> - Начать работу с опросом (переключиться в контекст)
  //Ответ бота - сообщение об успехе или ошибка
  def begin: Parser[Command] = "/begin" ~> "(" ~> """\d+""".r <~ ")" ^^ { d => Matcher.BeginM(d.toInt) }

  //  /end - Закончить работу с опросом (отключиться от контекста)
  //Ответ бота - сообщение об успехе или ошибка
  def end: Parser[Command] = "/end" ^^ { _ => Matcher.EndM() }

  //  /view - Просмотр информации об опросе (требует выбранного опроса)
  //Ответ бота - Красивое представление опроса
  def view: Parser[Command] = "/view" ^^ { _ => Matcher.ViewM() }


  //  /add_question <вопрос:string> [тип вопроса:open|choice|multi] - Добавление вопроса (требует выбранного опроса)
  //<Вариант ответа 1>
  //<Вариант ответа 2>
  //<Вариант ответа 3>
  //...
  //Ответ бота - номер добавленного вопроса либо понятная ошибка
  //Пояснения
  //  - открытый тип вопроса - вписать любую строку. При подведении
  //  итогов будут выданы все результаты
  //  - choice - выбор одного варианта. При подведении итогов будет
  //  выдана гистограмма по вариантам
  //  - multi - множественный выбор. При подведении итогов будет
  //  выдана гистограмма по вариантам
  def addQuestionOpen: Parser[Command] = {
    val question = Parser("(" ~> """[_a-zA-Zа-яА-ЯёЁ0-9.,:;'"*&!? ]+""".r <~ ")")
    val questionType = Parser("(" ~> "open" <~ ")")
    val command = Parser("/add_question" ~> question)
    command~questionType^^ { case a~b => Matcher.AddQuestionOpenM(a, b)}
  }

  def addQuestionChoiceOrMulti: Parser[Command] = {
    val question = Parser("(" ~> """[_a-zA-Zа-яА-ЯёЁ0-9.,:;'"*&!? ]+""".r <~ ")")
    val questionType = Parser("(" ~> ("choice"|"multi") <~ ")")
    val variant = Parser("\n" ~> """[_a-zA-Zа-яА-ЯёЁ0-9.,:;'"*&!? ]+""".r )
    val command = Parser("/add_question" ~> question)
    command~questionType~rep(variant) ^^ { case a~b~c => Matcher.AddQuestionChoiceOrMultiM(a, b, c)}
  }

  //  /delete_question <номер вопроса:digits> - Удаление вопроса (требует выбранного опроса)
  //Ответ бота - сообщение об успехе или ошибка
  def deleteQuestion: Parser[Command] =
    "/delete_question" ~> "(" ~> """\d+""".r <~ ")" ^^ { d => Matcher.DeleteQuestionM(d.toInt) }

  //  /answer <номер вопроса:digits> <ответ> - Ответить на вопрос (требует выбранного опроса)
  //Ответ бота - сообщение об успехе или ошибка
  //  Пояснения
  //    - формат ответа:
  //      open: <ответ на вопрос:string>
  //      choice: <номер ответа:digit>
  //      multi: <номера ответов:digit digit digit ... >
  //      естественно, ответа с указанным номером должен
  //      существовать, в случае mulit номер не должен повторяться
  //    - один пользователь может отвечать на один вопрос один раз
  def answerOpen: Parser[Command] = {
    val digit = Parser("(" ~> """\d+""".r <~ ")")
    val ans = Parser("(" ~> """[_a-zA-Zа-яА-ЯёЁ0-9.,:;'"*&!? ]+""".r <~ ")")
    val command = Parser("/answer" ~> digit)
    command~ans ^^ { case a~b => Matcher.AnswerOpenM(a.toInt, b)}
  }

  def answerChoiceOrMulti: Parser[Command] = {
    val digit = Parser("(" ~> """\d+""".r <~ ")")
    val variant = Parser("""\d+""".r)
    val command = Parser("/answer" ~> digit)
    command~rep(variant) ^^ { case a~b =>
      println(a, b)
      Matcher.AnswerChoiceOrMultiM(a.toInt, b.map(_.toInt))}
  }

  //  /help -  ну и добавим помощь что ли
  def help: Parser[Command] = "/help" ^^ { _ => Matcher.PrintHelpM() }

  val prs: Parser[Command] = createPoll | list | deletePoll | startPool | stopPoll | result |
    begin | end | view | answerOpen | answerChoiceOrMulti |
    deleteQuestion | addQuestionOpen | addQuestionChoiceOrMulti |
    help | failure(s"Команда не принята. Возможно, такой команды не существует. " +
                      s"Чтобы узнать список и синтаксис доступных команд, введите /help")

  //по сути, конструктор
  def apply(input: String): ParseResult[Command] = parse(prs, input)
}

//Для каждого класса можно объявить специальный объект-компаньон (companion object).
// Это объект, объявленный в том же файле и с тем же именем, что и класс.
// Этот объект имеет доступ к private полям класса.
// Позволяет, например, создавать экземпляры класса без использования new.
object CommandParser extends CommandParser