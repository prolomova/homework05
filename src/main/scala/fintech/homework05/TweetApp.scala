package fintech.homework05

import java.time.Instant
import java.util.UUID

/**
  * Вам необходимо реализовать api для создания твиттов, получения твитта и лайка твитта
  *
  * Создание твитта:
  * На вход к вам поступает CreateTweetRequest, из которого вы должны создать объект Tweet, обязательно проверив длину
  * текста (а может быть потом появятся и другие проверки).
  * hashTags вычисляется из tweet.text, собирая все слова, начинающиеся с символа `#`.
  * tweet.id генерируется из `UUID.randomUUID.toString`.
  * При условии прохождения всех проверок твит сохраняется в базу данных (вы можете реализовать любой способ сохранения:
  * в памяти, запись в файл, или то, что вам захочется).
  * После выполнения всех операций должен вернуться созданный объект.
  *
  * Получение твитта:
  * На вход к вам поступает GetTweetRequest, вернуть вы должны объект tweet, если он найдем по id.
  *
  * Лайк твитта:
  * Должен обновлять количество лайков у твитта и возвращать новое значение.
  * Если твит не найдет, то должна возвращаться ошибка
  *
  *
  * Все функции должны возвращать значение типа Result[T]
  * в котором может лежать либо текст ошибки, либо результат выполнение
  */

case class Tweet(id: String,
                 user: String,
                 text: String,
                 hashTags: Seq[String] = Seq.empty,
                 createdAt: Option[Instant] = None,
                 likes: Int)

case class CreateTweetRequest(text: String, user: String)

case class GetTweetRequest(id: String)

case class LikeRequest(id: String)

trait TweetStorage {
  def addTweet(tweet: Tweet): Result[Tweet]

  def getTweet(id: String): Result[Tweet]

  def updateTweet(tweet: Tweet): Result[Tweet]
}

sealed trait Result[+T]

final case class Success[T](result: T) extends Result[T]

final case class Error[T](errorMsg: String) extends Result[T]

class TwitterApi(val storage: TweetStorage) {
  final val maxTweetLength = 280

  private def isCorrectLength(len: Int): Boolean = len <= maxTweetLength && len != 0

  def createTweet(request: CreateTweetRequest): Result[Tweet] = request match {
    case CreateTweetRequest(_, _) if !isCorrectLength(request.text.length) =>
      Error("Tweet is empty or maximum length has been exceeded")
    case CreateTweetRequest(text, user) =>
      storage.addTweet(Tweet(UUID.randomUUID.toString, user, text,
        findHashTags(text).toList, Some(Instant.now), 0))
  }

  private def findHashTags(text: String): Seq[String] = {
    text.split(" ").filter(_.startsWith("#"))
  }

  def getTweet(request: GetTweetRequest): Result[Tweet] = storage.getTweet(request.id)

  def likeTweet(request: LikeRequest): Result[Tweet] = storage.getTweet(request.id) match {
    case Error(message) => Error(message)
    case Success(tweet: Tweet) => {
          val newTweet = tweet.copy(likes = tweet.likes + 1)
          storage.updateTweet(newTweet)
          Success(newTweet)
      }
  }
}


  class Storage extends TweetStorage {
    private var tweets: Map[String, Tweet] = Map.empty

    override def addTweet(tweet: Tweet): Result[Tweet] = tweets.get(tweet.id) match {
      case Some(_) => Error(s"A tweet with this id: ${tweet.id} is already in storage")
      case None =>
        tweets = tweets.updated(tweet.id, tweet)
        Success(tweet)
    }

    override def getTweet(id: String): Result[Tweet] = {
      tweets.get(id) match {
        case Some(tweet) => Success(tweet)
        case None => Error(s"Id: $id was not found")
      }
    }

    override def updateTweet(tweet: Tweet): Result[Tweet] = {
      tweets.get(tweet.id) match {
        case Some(_) =>
          tweets = tweets.updated(tweet.id, tweet)
          Success(tweet)
        case None => Error(s"A tweet with this id: ${tweet.id} is not in storage")
      }
    }
  }

  object TweetApiExample extends App {

    val storage: TweetStorage = new Storage()
    val app = new TwitterApi(storage)

    val request = CreateTweetRequest(user = "me", text = "Hello, world!")

    val response = app.createTweet(request)
    response match {
      case Success(value) => println(s"Created tweet with id: ${value.id}")
      case Error(message) => println(s"Failed to create tweet: $message")
    }

  }
