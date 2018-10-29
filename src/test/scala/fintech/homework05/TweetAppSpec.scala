package fintech.homework05

import org.scalatest.{FlatSpec, Matchers}

class TweetAppSpec extends FlatSpec with Matchers {
  val storage = new Storage()
  val app = new TwitterApi(storage)
  val tweet: Tweet = app
    .createTweet(CreateTweetRequest("I love Nutella #Nutella #NutellaLover", "NutellaLover"))
    .asInstanceOf[Success[Tweet]]
    .result

  "Created Tweet" should "be equal to the same Tweet" in {
    tweet should be(Tweet(
      tweet.id,
      "NutellaLover",
      "I love Nutella #Nutella #NutellaLover",
      Seq("#Nutella", "#NutellaLover"),
      tweet.createdAt,
      0))
  }

  "Created Tweet" should "be equal to Tweet from storage" in {
    val receivedTweet = GetTweetRequest(tweet.id)
    app.getTweet(receivedTweet).asInstanceOf[Success[Tweet]].result should be(tweet)
  }

  "Created Tweet" should "return correct Error if tweet's length is incorrect" in {
    val longTweet = "A" * app.maxTweetLength + "!"
    val emptyTweet = ""
    val tweet1 = app.createTweet(CreateTweetRequest(emptyTweet, "_"))
    val tweet2 = app.createTweet(CreateTweetRequest(longTweet, "AaA"))
    tweet1 should be(Error("Tweet is empty or maximum length has been exceeded"))
    tweet2 should be(Error("Tweet is empty or maximum length has been exceeded"))
  }

  "Created Tweet" should "correctly increase the number of likes" in {
    for (_ <- 0 until 5)
      app.likeTweet(LikeRequest(tweet.id))
    storage.getTweet(tweet.id).asInstanceOf[Success[Tweet]].result.likes should be(5)
  }

  "Created Tweet" should "return correct Error if tweet's id was not found" in {
    app.getTweet(GetTweetRequest("0")) should be(Error("Id: 0 was not found"))
    app.likeTweet(LikeRequest("?")) should be(Error("Id: ? was not found"))
  }
}