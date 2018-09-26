package Bot

// trait - по сути, интерфейс (пожалуй, ближе к абстрактным классам).
// Он пустой, а потом в него из матчера запихиваются все классы команд
trait Command

/*case class - синтаксический сахар. В нашем случае он принимает переменную и автоматически разворачивает её в класс,
у которого есть геттер и сеттер. Так мы поступим со всеми командами
*/
object Matcher {

  case class CreatePollM(name:String, anonymity:Option[String], continuous:Option[String],
                         start:Option[String], stop:Option[String]) extends Command
  case class ListM () extends Command
  case class DeletePollM(digit:Int) extends Command
  case class StartPollM(digit:Int) extends Command
  case class StopPollM(digit:Int) extends Command
  case class ResultM(digit:Int) extends Command
  case class BeginM(digit:Int) extends Command
  case class EndM() extends Command
  case class ViewM() extends Command
  case class DeleteQuestionM(digit:Int) extends Command
  case class AddQuestionOpenM(qName:String, qType:String) extends Command
  case class AddQuestionChoiceOrMultiM(qName:String, qType:String, variant:List[String]) extends Command
  case class AnswerOpenM(digit:Int,answer:String) extends Command
  case class AnswerChoiceOrMultiM(digit:Int,answer:List[Int]) extends Command
  case class PrintHelpM() extends Command
}
