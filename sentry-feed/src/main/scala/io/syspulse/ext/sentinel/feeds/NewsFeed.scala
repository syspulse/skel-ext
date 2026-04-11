package io.syspulse.ext.sentinel.feeds

import scala.util.Try
import scala.concurrent.{ExecutionContext, Future}

trait NewsFeed {
  def fetchFeed(timeout: Long = 0L)(implicit ec: ExecutionContext): Future[Seq[NewsPost]]
  def getSource(): String
  def getSourceType(): String  // "rss" or "reddit"
}
