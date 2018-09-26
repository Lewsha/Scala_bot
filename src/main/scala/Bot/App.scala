package Bot

import java.util.Date

/*
* По сути, классический case. Принимаем строку, парсим ее, затем применяем нужный метод в зависимости от результата
* Затем возвращает что-то в зависимости от результата
* */
object App {
  def responseToLine(line: String, user: User): String = {
    val result = CommandParser(line.trim)
    result.map {
      case line: Matcher.CreatePollM => Commands.createPoll(line.name,line.anonymity,line.continuous,line.start,line.stop, user)
      case line: Matcher.ListM => Commands.listPolls()
      case line: Matcher.DeletePollM => Commands.deletePoll(line.digit,user)
      case line: Matcher.StartPollM => Commands.startPoll(line.digit, new Date(), user)
      case line: Matcher.StopPollM => Commands.stopPoll(line.digit, new Date(), user)
      case line: Matcher.ResultM => Commands.pollResult(line.digit)
      case line: Matcher.BeginM => Commands.begin(line.digit, user)
      case line: Matcher.EndM => Commands.end(user)
      case line: Matcher.AddQuestionOpenM => Commands.addQuestion(line.qName,line.qType,"open"::Nil, user)
      case line: Matcher.AddQuestionChoiceOrMultiM => Commands.addQuestion(line.qName,line.qType,line.variant, user)
      case line: Matcher.DeleteQuestionM => Commands.deleteQuestion(line.digit, user)
      case line: Matcher.AnswerOpenM => Commands.addAnswerOpen(line.digit,line.answer, user)
      case line: Matcher.AnswerChoiceOrMultiM => Commands.addAnswerChoiceOrMulti(line.digit,line.answer, user)
      case line: Matcher.ViewM => Commands.view(user)
      case line: Matcher.PrintHelpM => Commands.help()
    }
      match {
        case CommandParser.Success(response, _) => response.toString
        case CommandParser.Failure(text, _) =>
          if (text.contains("expected")) s"```$text```" else text
        case CommandParser.Error(text, _) =>
          if (text.contains("expected")) s"```$text```" else text
    }
  }
}
