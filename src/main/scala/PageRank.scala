import scala.*
import scala.util.Random
import scala.collection.parallel.CollectionConverters.*

object PageRank {
    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return      A map of page.id to a weight of 1.0 for those same WebPage objects
     */
    def equal(pages: Map[String, WebPage]): Map[String, Double] = {
        (for element <- pages yield (element._1, 1.0)).toMap
    }

    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return A map of page.id to a weight that is a simple count of the number of pages linking to that page
     */
    def indegree(pages: Map[String, WebPage]): Map[String, Double] = {
        (for page <- pages yield {
            val count = (for otherPage <- pages yield {
                (for otherPageUrl <- otherPage._2.links if otherPageUrl == page._2.url yield 1).sum
            }).sum
            (page._1, count.toDouble)
        }).toMap
    }

    def pagerank(pages: Map[String, WebPage]): Map[String, Double] = {
        Map() // TODO: remove this stub and implement this method
    }
}