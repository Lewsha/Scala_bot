package Bot

import java.util.Date

// case class пула вопросов с нужными нам полями
case class Poll(name: String,                           // имя пула
                id: Int,                                // его идентификатор
                admin: Long,                            // пользователь, начавший этот пул
                anonymity: Boolean = true,              // режим анонимности пула
                continuousOrAfterstop: Boolean = false, // давать результат по окончанию опроса или в любой момент
                start_time: Option[Date] = None,        // время начала опроса
                end_time: Option[Date] = None,          // время окончания опроса
                questions: List[Question] = List(),     // список вопросов пула
               )


// внутренние команды для работы с пулом
object PollCommand {
  // добавление вопроса
  def addQuestion(poll: Poll, q:Question): Poll = poll.copy(questions = poll.questions :+ q)

  // обновление вопроса (используется при добавлении ответа
  def updateQuestion(poll: Poll, id:Int, q:Question): Poll =
    poll.copy(questions = poll.questions.updated(id, q))

  // удаление вопроса по ID
  def deleteQuestionById(poll: Poll, id:Int) : Poll =
    poll.copy(questions = poll.questions.filter(_ != poll.questions(id)))

  // возможно ли стартануть пулл?
  def pollCanNotBeStarted(poll: Poll, date: Date): Boolean =
    poll.end_time.exists(d => d.before(date))

  // активен ли пул?
  def active(poll: Poll, date: Date = new Date()): Boolean =
    if (poll.start_time.isEmpty) false
    else {
      val t = poll.end_time.getOrElse(new Date())
      t.compareTo(date) >= 0 && poll.start_time.get.before(date)
    }

  // запуск пула
  def start(poll: Poll, date: Date): Poll =
    if (poll.start_time.isDefined) poll
    else poll.copy(start_time = Option(date))

  // остановка пула
  def stop(poll: Poll, date: Date): Poll =
    if (poll.end_time.isDefined) poll
    else poll.copy(end_time = Option(date))

  // получить список вопросов
  def getView(poll: Poll) : String = {
    val result = for ((q, i) <- poll.questions.zipWithIndex) yield {
      q.typeOfQuestion match {
        case "multi" => s"$i) ${q.name} (multi):\n${getAggregatedOptions(q)}"
        case "choice" => s"$i) ${q.name} (choice):\n${getAggregatedOptions(q)}"
        case "open" => s"$i) ${q.name} (open)"
      }
    }
    s"В пуле '${poll.name}' ${poll.questions.size} вопрос(а)(ов):\n${result.mkString("\n")}"
  }

  // получить вариант ответа
  def getAggregatedOptions(question: Question): String = {
    question.variants.map(x => x.name).mkString("\n")
  }

  // получить результат опроса
  def getResult(poll: Poll, date: Date) : String = {
    if (active(poll, date) && !poll.continuousOrAfterstop) {
      s"Результаты опроса недоступны до _${poll.end_time.getOrElse(new Date())}_ "
  } else {
      val result = for (q <- poll.questions) yield {
        val questionResult = if (poll.anonymity) {
          q.typeOfQuestion match {
            case "multi" => getAnonChoiceResult(q)
            case "choice" => getAnonChoiceResult(q)
            case "open" => getAnonOpenResult(q)
          }
        } else {
          q.typeOfQuestion match {
            case "multi" => getNonAnonChoiceResult(q)
            case "choice" => getNonAnonChoiceResult(q)
            case "open" => getNonAnonOpenResult(q)
          }
        }
        getQuestionInfo(q) + questionResult
      }
      s"Результаты опроса *'${poll.name}'*:\n${result.mkString("\n")}"
    }
  }

  // получить количество ответивших на вопрос
  def getQuestionInfo(question: Question): String = {
    s"На вопрос `${question.name}` ответил(и) ${question.votedUsers.size} человек(а):"
  }

  // получить результаты открытого анонимного вопроса
  def getAnonOpenResult(question: Question): String = {
    question.variants.head.answers.aggregate("\n")((c,ans) =>  c + s"⋅    ${ans.answer}\n", _ + _)
  }

  // получить результаты открытого неанонимного вопроса
  def getNonAnonOpenResult(question: Question): String = {
    question.variants.head.answers.aggregate("\n")((c,ans) =>  c + s"⋅    ${ans.user.get.name}: ${ans.answer}\n", _ + _)
  }

  // получить результаты анонимного вопроса с единичным и мультивыбором
  def getAnonChoiceResult(question: Question): String = {
    question.variants.aggregate("\n")((b,variant) => b + s"⋅    ${variant.name}: _${variant.answers.size}_\n", _ + _)
  }

  // получить результаты неанонимного вопроса с единичным и мультивыбором
  def getNonAnonChoiceResult(question: Question): String = {
    question.variants.aggregate("\n")((b,variant) => b + s"⋅    ${variant.name}: _${variant.answers.size}_\n" +
      variant.answers.aggregate("")((k, ans) => k + s"⁘      _(${ans.user.get.name})_", _ + _) + "\n", _ + _)
  }
}