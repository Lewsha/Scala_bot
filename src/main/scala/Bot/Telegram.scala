package Bot

// используем bot wrapper for Scala от bot4s
import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.api.declarative._
import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._


object Telegram extends TelegramBot with Polling with Commands {
  // токен нашего бота, выданный Телеграммом (метод уже определен в используемой библиотеке)
  def token = "625254483:AAFWUOTp38AISxwoDvoGOeievAix2fzWq90"


  // метод посылки сообщений
  override def receiveMessage(msg: Message): Unit = {
    val name = s"${msg.from.get.firstName} ${msg.from.get.lastName.getOrElse("")}".trim // получаем из
    // принятого сообщения имя пользователя
    val user = Bot.User(msg.source, name) // создаем пользователя
    for (text <- msg.text) {
      println(s"Text received: '$text', from: '$name', id: ${msg.from.get.id}")  // принтуем что и от кого мы получили
      request(SendMessage(msg.source, App.responseToLine(text, user), Some(ParseMode.Markdown))) // обрабатываем
      // сообщение от пользователя и отвечаем
    }
  }

  //как бы запуск
  def main(a: Array[String]): Unit = {
    println("Started telegram bot")
    run()
  }
}
