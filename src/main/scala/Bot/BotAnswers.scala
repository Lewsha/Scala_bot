package Bot
//тут забиты все текстовые ответы нашего бота

object BotAnswers {
  def noSuchPoll = s"Не вижу такой пул вопросов. Возможно, его не существует"
  def noPermissions = s"У вас нет прав на данную операцию"
  def youForgotToBegin = s"Вы забыли ввести команду /begin"
  def noPolls = s"Больше опросов нет"
  def noSuchQuestion = s"Нет такого вопроса"
  def listPolls(polls: Iterable[Poll]): String = {
    val pollStrings = polls.map(poll => s"${poll.id}) ${poll.name}")
    s"Текущие пулы вопросов:\n${pollStrings.mkString("\n")}"
  }
  def youAlreadySelectedThisPoll = s"Вы уже в выбранном пуле вопросов"
  def pollWasDeleted = s"Пул успешно удален"
  def pollIsRunning = s"Опрос уже запущен"
  def pollWasStarted = s"Опрос запущен"
  def cantStartPoll(id: Int) = s"Не могу запустить опрос № $id снова"
  def pollIsNotActive = s"Опрос не активен"
  def pollIsStopped = s"Пул остановлен"
  def pollWillStartAutomatically = s"Пул будет запущен автоматически"
  def afterBeginHint: String = s"Теперь вы можете:" +
    s"\n/add\\_question _(<question>)_ _(open|choice|multi)_, - создать вопрос" +
    s"\n/delete\\_question _(<question number>)_, - удалить вопрос" +
    s"\n/answer _(<question number>)_ _(<answer>)_ or - ответить на вопрос" +
    s"\n/view all questions - посмотреть все вопросы" +
    s"\nИ не забудьте завершить работу командой /end"
  def afterEndHint(pollId: Int) = s"Теперь у вас есть /result ($pollId) "
  def questionWasAdded(name: String, id: Int) = s"Вопрос '$name' добавлен ($id)"
  def questionWasDeleted = s"Вопрос был удален"
  def cantVoteTwice = s"Вы не можете голосовать дважды!"
  def badQuestionType(typeOfQuestion: String) = s"Это вопрос типа $typeOfQuestion"
  def thanksForVoting = "Спасибо за ваш ответ!"
  def pollWasCreated(id: Int, name: String): String =
    s"Опрос *$name* был создан, его номер:\n `$id` \n" +
      s"Используйте /begin ($id), чтобы продолжить"
}
