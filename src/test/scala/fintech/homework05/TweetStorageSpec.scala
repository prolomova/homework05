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
    returnedTweet.asInstanceOf[Success[Tweet]].result should be(tweet)
  }

  "Storage" should "return correct Error if Tweet with id was not found" in {
    storage.getTweet("1") should be(Error("Id: 1 was not found"))
  }

  "Storage" should "return correct Error if tweet's id is already in storage" in {
    storage.addTweet(tweet) should be(Error("A tweet with this id: 0 is already in storage"))
  }

  "Storage" should "return correct Error if updated tweet's id is not in storage" in {
    storage.updateTweet(tweet.copy(id = "1")) should be(Error("A tweet with this id: 1 is not in storage"))
  }

  "Storage" should "update tweet correctly" in {
    val newTweet = storage.updateTweet(tweet.copy(likes = 1)).asInstanceOf[Success[Tweet]].result
    storage.getTweet(newTweet.id).asInstanceOf[Success[Tweet]].result should be(newTweet)
  }
}
