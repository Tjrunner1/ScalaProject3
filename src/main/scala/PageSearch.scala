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
//        @tailrec
//        def rec(text: String, stringIndex: Int, count: Int): Int = {
//            if (text) {
//
//            }
//
//        }
//        pages.map(page => {
//            for x <- page.text.length yield if
//        })
//        pages(0).text.indexOf("Bible", 2)

        for searchString <- query yield {
            (for webpage <- pages yield {
                (for index <- 0 to webpage.text.length - searchString.length if webpage.text.substring(index, index + searchString.length) == searchString yield 1).length
            }).sum
        }
//        List() // TODO: implement this method and remove this stub
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the term-frequency of the occurrences of those terms in each page in the same order given
     */
    def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        List() // TODO: implement this method and remove this stub
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the TF-IDF score for each page in the same order given
     */
    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        List() // TODO: implement this method and remove this stub
    }
}