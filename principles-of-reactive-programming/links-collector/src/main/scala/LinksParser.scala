package linksCollector

object LinksParser {
  val A_TAG = "(?i)<a ([^>]+)>.+?</a>".r
  val HREF_ATTR = """\s*(?i)href\s*=\s*(?:"([^"]*)"|'([^']*)'|([^'">\s]+)""".r

  def findLinks(body: String): Iterator[String] = {
    for {
      anchor <- A_TAG.findAllMatchIn(body)
      HREF_ATTR(doublequotes, quotes, bare) <- anchor.subgroups
    } yield
        if (doublequotes != null) {
          doublequotes
        } else if (quotes != null) {
          quotes
        } else {
          bare
        }
  }
}