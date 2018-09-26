package Bot

import java.text.SimpleDateFormat
import java.util.Date

import scala.collection.immutable
import scala.collection.immutable.HashSet

import Bot.Commands.getContextById

/*
* Репозиторий (по сути, структура данных, хранящая пулы)
*/
trait Repository {
  var polls: Map[Int, Poll] = immutable.Map[Int, Poll]() // коллекция, содержащая пул и идентификатор.
  // Делаем ее неизменяемой (требования)

  def updatePoll(pollId: Int, poll: Poll): Unit = polls += pollId -> poll // добавить пул по ключу

  def removePoll(pollId: Int): Unit = polls -= pollId // удалить пул по ключу

  def getPollById(id: Int): Either[String, Poll] = // вернуть пул по ключу (или не вернуть)
    if (polls.contains(id)) Right(polls(id)) else Left(BotAnswers.noSuchPoll)

  def getPollForUser(pollId: Int, userId: Long): Either[String, Poll] = { // получить пул по ключу (или нет)
    getPollById(pollId).flatMap(poll =>
      if (poll.admin == userId) Right(poll) else Left(BotAnswers.noPermissions))
  }

  def getQuestionForUser(questionId: Int, user: User): Either[String, (Poll, Question)] = // получить вопрос
    getContextById(user.id).flatMap(getPollById).flatMap(poll => {
      if (questionId < 0 || questionId >= poll.questions.size)
        Left(BotAnswers.noSuchQuestion)
      else {
        val question = poll.questions(questionId)
        if (question.votedUsers.contains(user))
          Left(BotAnswers.cantVoteTwice)
        else
          Right((poll, question))
      }
    })
}

/*
* Контекст (состояние, в котором находится каждый конкретный пользователь внутри бота (в каком пуле, в каком вопросе,
* на каком этапе))
* */
trait Context {
  var context: Map[Long, Int] = immutable.Map[Long, Int]() // коллекция, содержащая идентификатор пользователя
  // (получаем от телеги) и идентификатор пула, в котором он находится.
  // Делаем ее неизменяемой (требования)

  def setContext(userId: Long, context: Int) {  // добавить контекст в коллекцию
    this.context += userId -> context
  }

  def removeContext(userId: Long): Unit = context -= userId  // удаляем контекст
  // (когда пользователь сваливает из опроса через команду /end)

  def isInContext(userId: Long): Boolean = context.contains(userId)  // А есть ли такой контекст?

  def getContextById(id: Long): Either[String, Int] =  // получить контекст по идентификатору пользователя
    if (!context.contains(id)) Left(BotAnswers.youForgotToBegin)
    else Right(context(id))
}

// определяем непосредственно работу команд
object Commands extends Repository with Context {

  val formatDate = new SimpleDateFormat("hh:mm:ss yy:MM:dd")  // определяем формат даты,
  // с которым будем работать

  val maxId: Iterator[Int] = Stream.from(0).iterator  // задаем максимальный идентификатор для пулов
  // (мы используем итератор, так что он может быть ОООООЧЕНЬ большим)

  val userID: Iterator[Int] = Stream.from(0).iterator  // то же для пользователей

  def getMaxID: Int = maxId.next()  // получаем следующий идентификатор при надобности

  def parseTime(time: Option[String]): Option[Date] =  // парсим время из строки
  // (или не парсим, если время не определено)
    if (time.isDefined)
      Option(formatDate.parse(time.getOrElse(formatDate.format(new Date))))
    else
      None

  def getTimeFromFormat(string: String): Date = formatDate.parse(string)  // получаем дату

  // создаем новый пул
  def createPoll(name: String, anonymityVar: Option[String], continuousOrAfterstopVar: Option[String],
                 startTimeVar: Option[String], stopTimeVar: Option[String], user: User = User(0, "")): String = {
    val anonymity = anonymityVar.getOrElse("yes") == "yes"
    val continuousOrAfterstop = continuousOrAfterstopVar.getOrElse("afterstop") == "continuous"
    val id = getMaxID
    updatePoll(id, Poll(name, id, user.id, anonymity,
      continuousOrAfterstop, parseTime(startTimeVar), parseTime(stopTimeVar)))

    BotAnswers.pollWasCreated(id, name)
  }

  // отдаем список пулов
  def listPolls(): String =
    if (polls.isEmpty)
      BotAnswers.noPolls
    else
      BotAnswers.listPolls(polls.values)


  // удаляем пул
  def deletePoll(id: Int, user: User): String =
    getPollForUser(id, user.id).map(_ => {
      removePoll(id)
      BotAnswers.pollWasDeleted
    }).merge

  // стартуем опрос
  def startPoll(id: Int, date: Date, user: User): String =
    getPollForUser(id, user.id).map(poll => {
      if (PollCommand.pollCanNotBeStarted(poll, date)) {
        BotAnswers.cantStartPoll(id)
      } else if (PollCommand.active(poll, date) || poll.start_time.isDefined) {
        BotAnswers.pollIsRunning
      } else {
        updatePoll(id, PollCommand.start(poll, date))
        BotAnswers.pollWasStarted
      }
    }).merge

  // останавливаем опрос
  def stopPoll(id: Int, date: Date, user: User): String =
    getPollForUser(id, user.id).map(poll => {
      if (!PollCommand.active(poll, date)) {
        BotAnswers.pollIsNotActive
      } else if (poll.end_time.isEmpty) {
        updatePoll(id, PollCommand.stop(poll, date))
        BotAnswers.pollIsStopped
      }
      else {
        BotAnswers.pollWillStartAutomatically
      }
    }).merge

  // получаем результаты опроса
  def pollResult(id: Int): String =
    getPollById(id).map(poll => PollCommand.getResult(poll, new Date)).merge


  // начинаем работу с опросом
  def begin(id: Int, user: User): String =
    getPollById(id).map(_ =>
      if (context.get(user.id).contains(id))
        BotAnswers.youAlreadySelectedThisPoll
      else {
        setContext(user.id, id)
        BotAnswers.afterBeginHint
      }
    ).merge

  // заканчиваем работу с опросом
  def end(user: User): String =
    getContextById(user.id).map(pollId => {
      removeContext(user.id)
      BotAnswers.afterEndHint(pollId)
    }).merge

  // получаем список вопросов
  def view(user: User): String =
    getContextById(user.id).flatMap(getPollById).map(PollCommand.getView).merge

  def addQuestion(name: String, typeOfQuestion: String, list: List[String], user: User): String =
    getContextById(user.id)
      .flatMap(pollId => getPollForUser(pollId, user.id))
      .map(poll => {
        val question = Question(name, typeOfQuestion, HashSet[User](), list.map(e => Variant(e, Nil)))
        updatePoll(poll.id, PollCommand.addQuestion(poll, question))
        BotAnswers.questionWasAdded(name, poll.questions.size)
      }).merge

  def deleteQuestion(questionId: Int, user: User): String =
    getQuestionForUser(questionId, user).map {
      case (poll, _) =>
        updatePoll(poll.id, PollCommand.deleteQuestionById(poll, questionId))
        BotAnswers.questionWasDeleted
    }.merge

  // добавляем ответ на вопрос типа open
  def addAnswerOpen(questionId: Int, answer: String, user: User): String =
    getQuestionForUser(questionId, user).map {
      case (poll, question) =>
        // если ошибочка с типом вопроса, то выдаем ошибку
        if (question.typeOfQuestion != "open")
          BotAnswers.badQuestionType(question.typeOfQuestion)
          // если пул не активен, то выдаем ошибку
        else if (!PollCommand.active(poll)) BotAnswers.pollIsNotActive
          // если все ок, то добавляем наш ответ и апдейтим пул
        else {
          val updatedQuestion = QuestionHandler.addAnswerOpen(poll.questions(questionId),
            poll.anonymity, 0, Answer(answer, Some(user)))
          updatePoll(poll.id, PollCommand.updateQuestion(polls(poll.id), questionId, updatedQuestion))
          BotAnswers.thanksForVoting
        }
    }.merge

  // добавляем ответ на вопрос с выбором (и choice, и multi)
  def addAnswerChoiceOrMulti(questionId: Int, list: List[Int], user: User): String =
    getQuestionForUser(questionId, user).map {
      case (poll, question) =>
        // если у нас вопрос типа choice, но ответ не один, или ответов нет, или тип вопроса open, то выдаем ошибку
        if ((question.typeOfQuestion == "choice" && list.size != 1) || list.isEmpty ||
          question.typeOfQuestion == "open")
          BotAnswers.badQuestionType(question.typeOfQuestion)
          // если пул не активен, то выдаем сообщение
        else if (!PollCommand.active(poll)) BotAnswers.pollIsNotActive
          // если же все ок, то проходим по списку ответов и для каждого апдейтим наш вопрос, добавляя ответы.
        // Соответственно, на choice это будет сделано только раз, а на multi - по числу ответов и один раз
        // добавляется проголосовавший в список (если анонимность, то будет просто None).
        // Ну и после этого апдейтим пул
        else {
          for (i <- list) yield {
            val updatedQuestion = QuestionHandler.addAnswerChoiceOrMulti(polls(poll.id).questions(questionId),
              poll.anonymity, i, Answer("", Option(user)))
            updatePoll(poll.id, PollCommand.updateQuestion(polls(poll.id), questionId, updatedQuestion))
          }
          val updatedQuestion = QuestionHandler.addVotedUser(polls(poll.id).questions(questionId),
            Answer("", Option(user)))
          updatePoll(poll.id, PollCommand.updateQuestion(polls(poll.id), questionId, updatedQuestion))
          BotAnswers.thanksForVoting
        }
    }.merge

  // просто выводим пользователю список доступных команд с возможным синтаксисом
  def help(): String =
    s"Список доступных команд\n" +
      s"/create\\_poll (Название опроса) [(анонимность: yes|no) " +
      s"(видимость результата:afterstop|continuous) " +
      s"(время начала: чч:мм:сс гг:мм:дд) (время окончания)]" +
      s"  - создать новый пул вопросов\n" +
      s"/list - увидеть текущие опросы\n" +
      s"/delete\\_poll (id опроса) - удалить опрос\n" +
      s"/start\\_poll (id опроса) - запустить пул\n" +
      s"/stop\\_poll (id опроса) - остановить пул\n" +
      s"/result (id опроса) - просмотр результатов\n" +
      s"/begin (id опроса) - начать работу с опросом\n\n" +
      s"После команды */begin* станут доступны следующие команды\n:" +
      s"/view - посмотреть список вопросов\n" +
      s"/add\\_question (вопрос) [(тип вопроса: open|choice|multi)]\n " +
      s"<Вариант ответа 1>" +
      s"<Вариант ответа 2>" +
      s"<Вариант ответа 3>" +
      s" - добавить вопрос в пул\n" +
      s"/delete\\_question (id вопроса) - удалить вопрос из текущего пула\n" +
      s"/answer (номер вопроса) (ответ) - ответить на вопрос\n" +
      s"/end - покинуть текущий опрос"
}
