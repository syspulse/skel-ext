package io.syspulse.ext.sentinel.feeds

import scala.util.Try

trait NewsFeed {
  def fetchFeed(): Try[Seq[NewsPost]]
  def getSource(): String
  def getSourceType(): String  // "rss" or "reddit"
}
