package Bot

import org.scalatest.{FlatSpec, Matchers}



class CommandParserTest extends FlatSpec with Matchers{
  private val parser = CommandParser

  "/create_poll (first_poll) (no) (afterstop))" should Matcher.CreatePollM.toString() in {
    val result = parser("/create_poll (first_poll) (no) (afterstop))")
    result.get match {
      case poll: Matcher.CreatePollM =>
        poll.name shouldBe "first_poll"
        poll.anonymity shouldBe Some("no")
    }
    result.get shouldBe a[Matcher.CreatePollM]
  }


  "/create_poll (second_poll) (yes) (continuous) (12:00:00 18:09:01)" should Matcher.CreatePollM.toString() in {
    val result = parser("/create_poll (second_poll) (yes) (continuous) (12:00:00 18:09:01)")
    result.get match { case c: Matcher.CreatePollM =>
      c.name shouldBe "second_poll"
      c.anonymity shouldBe Some("yes")
    }
    result.get shouldBe a [Matcher.CreatePollM]
  }

  "/create_poll (third_poll) (yes) (continuous) (12:00:00 18:09:01) (15:00:00 18:09:01)" should
    Matcher.CreatePollM.toString() in {
    val result = parser("/create_poll (third_poll) (yes) (continuous) (12:00:00 18:09:01) (15:00:00 18:09:01)")
    result.get shouldBe a [Matcher.CreatePollM]
  }

  "/list" should Matcher.ListM.toString() in {
    val result = parser("/list")
    result.get shouldBe a [Matcher.ListM]
  }

  "/delete_poll (0)" should Matcher.DeletePollM.toString() in {
    val result = parser("/delete_poll (0)")
    result.get match { case command: Matcher.DeletePollM => command.digit shouldBe 0}
    result.get shouldBe a [Matcher.DeletePollM]
  }

  "/start_poll (0)" should Matcher.StartPollM.toString() in {
    val result = parser("/start_poll (0)")
    result.get match { case command: Matcher.StartPollM => command.digit shouldBe 0}
    result.get shouldBe a [Matcher.StartPollM]
  }

  "/stop_poll (2)" should Matcher.StopPollM.toString() in {
    val result = parser("/stop_poll (2)")
    result.get match { case command: Matcher.StopPollM => command.digit shouldBe 2}
    result.get shouldBe a [Matcher.StopPollM]
  }

  "/result (0)" should Matcher.ResultM.toString() in {
    val result = parser("/result (0)")
    result.get match { case command: Matcher.ResultM => command.digit shouldBe 0}
    result.get shouldBe a [Matcher.ResultM]
  }


  "/bad_request" should "Fail" in {
    val result = parser("/bad_request")
    result.successful shouldBe false
  }


  "/begin (2)" should Matcher.BeginM.toString() in {
    val result = parser("/begin (2)")
    result.get match { case command: Matcher.BeginM => command.digit shouldBe 2}
    result.get shouldBe a [Matcher.BeginM]
  }

  "/end " should Matcher.EndM.toString() in {
    val result = parser("/end")
    result.get shouldBe a [Matcher.EndM]
  }

  "/view " should Matcher.ViewM.toString() in {
    val result = parser("/view ")
    result.get shouldBe a [Matcher.ViewM]
  }

  "/add_question open" should Matcher.AddQuestionOpenM.toString() in {
    val result = parser("""/add_question (Test question) (open)""".stripMargin)
    result.get match { case command: Matcher.AddQuestionOpenM =>
      command.qName shouldBe "Test question"
      command.qType shouldBe "open"
    }
    result.get shouldBe a [Matcher.AddQuestionOpenM]
  }

  "/delete_question " should Matcher.DeleteQuestionM.toString() in {
    val result = parser("/delete_question (0)")
    result.get match { case command: Matcher.DeleteQuestionM => command.digit shouldBe 0}
    result.get shouldBe a [Matcher.DeleteQuestionM]
  }

  "/answer string" should Matcher.AnswerChoiceOrMultiM.toString() in {
    val result = parser("""/answer (0) (answer)""")
    result.get match { case command: Matcher.AnswerOpenM =>
      command.digit shouldBe 0
      command.answer shouldBe "answer"
    }
    result.get shouldBe a [Matcher.AnswerOpenM]
  }
}