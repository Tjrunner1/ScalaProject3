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
        println("MAP LENGTH" + pages.size)
        (for page <- pages yield { //for each page yield...
            val count = (for otherPage <- pages yield { //for every otherPage(the same list of pages) yield ...
                (for otherPageUrl <- otherPage._2.links if otherPageUrl == page._1 yield 1).sum //Count the number of url's that are equal to page.id (from otherPage's Url List)
            }).sum //sum the total number of times page.id was found in all the pages
            (page._1, count.toDouble) //return a Map[String, Double]
        }).toMap
    }

    def pagerank(pages: Map[String, WebPage]): Map[String, Double] = {
        val rand = scala.util.Random
        val pagesLandedOn = (for x <- 0 until 10000 yield {
            getWalk(pages, pages.toList(rand.between(0, pages.size))._2)
        }).toList

        for page <- pages yield {
            (page._1, ((pagesLandedOn.count(p => page._2 == p) + 1) / (10000 + pages.size)).toDouble)
        }


    }

    def getWalk(pages: Map[String, WebPage], chosenPage: WebPage): WebPage = {
        def helper(loopCount: Int, pages: Map[String, WebPage], chosenPage: WebPage): WebPage = {
            loopCount match {
                case 0 => chosenPage
                case _ => helper(loopCount-1, pages,pages.getOrElse(getChosen(pages, chosenPage), WebPage("","","","", List(""))))
            }
        }
        helper(100, pages, chosenPage)
    }

    def getChosen(pages: Map[String, WebPage], page: WebPage) = {
        val rand = scala.util.Random
        val followLinkChance = rand.between(0, 100)
        if(followLinkChance > 15 && page.links.length > 0) {
            val pageLink = rand.between(0, page.links.length)
            page.links(pageLink)
        } else {
            val randPage = rand.between(0, pages.size)
            pages.toList(randPage)._1
        }
    }
}