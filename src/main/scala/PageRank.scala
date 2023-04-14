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
        (for page <- pages yield { //for each page yield...
            val count = (for otherPage <- pages yield { //for every otherPage(the same list of pages) yield ...
                (for otherPageUrl <- otherPage._2.links if otherPageUrl == page._2.url yield 1).sum //Count the number of url's that are equal to page.url (from otherPage's Url List)
            }).sum //sum the total number of times page.url was found in all the pages
            (page._1, count.toDouble) //return a Map[String, Double]
        }).toMap
    }

    def pagerank(pages: Map[String, WebPage]): Map[String, Double] = {
        Map() // TODO: remove this stub and implement this method
    }
}