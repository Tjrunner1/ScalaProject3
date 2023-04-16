import scala.annotation.tailrec
import scala.math.log
import scala.collection.parallel.CollectionConverters.*

object PageSearch {
    /**
     * @param pages  a list of RankedWebPage objects to be searched
     * @param query  a list of search terms to be counted in those pages
     * @return       a list of the number of times any of the terms appeared in each page in the same order as given
     */
    def count(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        for webpage <- pages yield {
            (for searchString <- query yield {
                (for index <- 0 to webpage.text.length - searchString.length if webpage.text.substring(index, index + searchString.length).toLowerCase() == searchString.toLowerCase() yield 1).length
            }).sum
        }
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the term-frequency of the occurrences of those terms in each page in the same order given
     */
    def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        val counts = count(pages, query) //Use count function to find the number of times a query appeared for each page
        (for index <- 0 until pages.length yield counts(index)/pages(index).text.length).toList //loop through all the pages and return (the total number of times a query appeared in a page) divided by (the total length of the page) //(This output will be the weight a page recieves)
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the TF-IDF score for each page in the same order given
     */
    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        val typeFrequency = tf(pages, query)
        val counts = count(pages, query).filter(x => x > 0)
        (for index <- 0 until pages.length yield typeFrequency(index) * (log(pages.length/(counts.length + 1)))).toList

    }
}