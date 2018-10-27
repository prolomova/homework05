package fintech.homework05

import java.time.Instant

import org.scalatest.{FlatSpec, Matchers}

class TweetStorageSpec extends FlatSpec with Matchers {
  val storage = new Storage()
  val tweet = Tweet("0",
    "NutellaLover",
    "I love Nutella #Nutella #NutellaLover",
    Seq("#Nutella", "#NutellaLover"),
    Some(Instant.now),
    0)

  "Storage" should "add Tweet correctly" in {
    storage.addTweet(tweet) should be(Success(tweet))
  }

  "Storage" should "return tweet from Storage correctly" in {
    val storage = new Storage()
    storage.addTweet(tweet)
    val returnedTweet = storage.getTweet("0")
    returnedTweet.value should be(tweet)
  }

  "Storage" should "return correct Error if Tweet with id was not found" in {
    storage.getTweet("1") should be(Error("Id: 1 was not found"))
  }

  "Storage" should "return correct Error if tweet's id is already in storage" in {
    storage.addTweet(tweet) should be(Error("A tweet with this id: 0 is already in storage"))
  }

  "Storage" should "increase Tweet's like correctly" in {
    storage.increaseLike(tweet) should be(Success(Tweet(
      tweet.id,
      tweet.user,
      tweet.text,
      tweet.hashTags,
      tweet.createdAt,
      tweet.likes + 1)))
  }

  "Storage" should "return correct Error if Tweet with such id does not exist" in {
    val tweet = Tweet("1",
      "Tester",
      "More tests #test",
      Seq("#test"),
      Some(Instant.now),
      0)
    storage.increaseLike(tweet) should be(Error("Tweet with id: 1 does not exist"))
  }
}
