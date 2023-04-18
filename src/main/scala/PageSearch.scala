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
        //Get count of each term on a webpage
        def singlePageCount(page: RankedWebPage, query: List[String]): List[Double] = {
            for searchString <- query yield {
                (for index <- 0 to page.text.length - searchString.length if page.text.substring(index, index + searchString.length).toLowerCase() == searchString.toLowerCase() yield 1).length
            }
        }
        //get tf for each term on a page
        def singlePagetf(page: RankedWebPage, query: List[String]): List[Double] = {
            val counts = singlePageCount(page, query)
            (for index <- 0 until query.length yield {
                counts(index)/page.text.length
            }).toList
        }
        
        val idfs = for term <- query yield {
            log(pages.length / (count(pages, List(term)).count(x => x > 0) + 1))
        }
        for page <- pages yield {
            val tfs = singlePagetf(page, query)
            val dotProduct = (for i <- 0 until query.length yield {
                idfs(i) * tfs(i)
            }).sum //calculates dot product
            dotProduct
        }
//        val typeFrequency = tf(pages, query)
//        val counts = count(pages, query)) //looking at each term instead of the whole query
//        (for index <- 0 until pages.length yield typeFrequency(index) * (log(pages.length/(counts.length + 1)))).toList

    }
}