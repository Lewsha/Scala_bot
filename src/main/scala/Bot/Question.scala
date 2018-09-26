package Bot

// будем использовать неизменяемые хэш-множества
import scala.collection.immutable.HashSet

// вопрос
case class Question(name: String, typeOfQuestion: String, votedUsers:HashSet[User], variants: List[Variant])
// пользователь
case class User(id:Long, name:String)
// ответ
case class Answer(answer: String, user: Option[User])
// вариант ответа
case class Variant(name: String, answers: List[Answer])

// методы для работы с вопросами
object QuestionHandler {

  // метод добавления ответа
  def addAnswerOpen(question: Question, anonymity:Boolean, id:Int, answer: Answer): Question = {
    if (anonymity) {
      // если включена анонимность, то мы отдаем вопрос, в котором у ответов пользователи будут переведны в None
      val ans = question.variants(id).copy(answers = answer.copy(user = None) :: question.variants(id).answers)
      question.copy(variants = question.variants.updated(id, ans)).copy(votedUsers = question.votedUsers
        + answer.user.get)
    } else {
      // если же все ок, то просто копируем
      val ans = question.variants(id).copy(answers = answer :: question.variants(id).answers)
      question.copy(variants = question.variants.updated(id, ans)).copy(votedUsers = question.votedUsers
        + answer.user.get)
    }
  }

  // метод добавления ответа на choice и multi
  def addAnswerChoiceOrMulti(question: Question, anonymity: Boolean, id: Int, answer: Answer): Question =
    if (anonymity) {
      // такая же петрушка с анонимностью
      val a = question.variants(id).copy(answers = answer.copy(user = None) :: question.variants(id).answers)
      question.copy(variants = question.variants.updated(id, a))
    } else {
      val a = question.variants(id).copy(answers = answer :: question.variants(id).answers)
      question.copy(variants = question.variants.updated(id, a))
    }

  // метод добавления проголосовавших (при новом ответе на какой-то вариант копируем вопрос,
  // а к списку проголосовавших добавляем нового проголосовавшего)
  def addVotedUser(question: Question, answer: Answer): Question = {
    question.copy(votedUsers = question.votedUsers + answer.user.get)
  }

}