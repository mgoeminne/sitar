package com.github.mgoeminne.sitar

import java.io.{InputStream, FileInputStream, File}
import java.net.URL

import com.github.mgoeminne.sitar.parser.{ieeetr, acm}

import org.apache.pdfbox.pdfparser.PDFParser
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.util.PDFTextStripper
import org.jsoup.Jsoup
import scala.collection.JavaConversions._

/**
  * http://www.dlib.org/dlib/september13/kern/09kern.html
  * https://clgiles.ist.psu.edu/pubs/LREC2008-ParsCit.pdf
  */
object Main
{
   def findPaper(p: Paper): Option[URL] =
   {
      val url = """https://scholar.google.com/scholar?q="""" + p.title.replaceAll(" ", "+") + """""""
      println(url)

      val doc =
         Jsoup.connect(url)
            .data("query", "Java")
            .userAgent("Mozilla")
            .cookie("auth", "token")
            .timeout(3000)
            .get();

      val entries = doc.select("div.gs_r")

      val candidates = entries.map(entry => {
          val title = entry.select("h3.gs_rt").first().text
                           .replaceAll("""\[[^\]]*\]""", "").trim


          val authors = entry.select("div.gs_a").first.text
                             .takeWhile(c => c != '-')
                             .split(',')
                             .map(author => author .split(' ')
                                                   .last
                                                   .replaceAll("""[^\w]""", "")
                                                   .trim
                             )
                             .filterNot(_.isEmpty)

          val url = Option(entry.select("div#gs_ggsW0").first())
                     .map(element => element.select("a[href]").first.attr("abs:href"))
         (Paper(title, authors) , url)
      })

      candidates.find(c => Paper.similar(c._1, p) && c._2.isDefined)
                .map(c => new URL(c._2.get))
   }

   def main(args: Array[String])
   {
      val paper = Paper("A study of library migration in java software",
                        Seq("Teyton", "Falleri", "Palyart", "Blanc"))

      val url = new URL("http://arxiv.org/pdf/1306.6262")
      //extractCitations(url.openStream())
      //extractCitations(new FileInputStream(new File("/Users/mg/Downloads/b.pdf")))
      //extractCitations(new FileInputStream(new File("/Users/mg/Downloads/1306.6262.pdf")))
      //val citations = extractCitations(new FileInputStream(new File("/Users/mg/Documents/doctorat/ressources électroniques/classés/phdthesis/Goeminne - Understanding the Evolution of Socio-technical Aspects in Open Source Ecosystems An Empirical Analysis of GNOME.pdf")))
      val citations = extractCitations(new FileInputStream(new File("/Users/mg/Documents/doctorat/ressources électroniques/classés/inproceedings/Capiluppi - Developing an h-index for OSS developers.pdf")))

      val styleParsers = Seq(ieeetr.parser, acm.parser)
      styleParsers foreach println

      //citations.map(ieeetr.parser.parse) foreach println
   }

   def extractCitations(stream: InputStream): Seq[String] =
   {
      val content = extractContent(stream)
      extractReferenceContent(content)
   }

   /**
     * Extracts a textual representation of the references
     * @param content a pdf content
     * @param threshold the minimal position (between 0 and 1) the
     *                  bibliography section must starts. Any sections before this position are ignored
     * @return the part of the content that relates to the document references
     */
   def extractReferenceContent(content: Seq[String], threshold:Float = 0.4f): Seq[String] =
   {
      val limit = (content.size * threshold).toInt
      val startLabels = """^(References?|REFERENCES?|Bibliography|BIBLIOGRAPHY|References?\s+and\s+Notes?|References?\s+Cited|REFERENCES?\s+CITED|REFERENCES?\s+AND\s+NOTES?):?\s*$""".r
      val endLabels = """^(Acknowledg(e?)ments?|ACKNOWLED(E?)GMENTS?|Index|INDEX|Biograph(y|ies)|BIOGRAPH(Y|IES)|Appendix|APPENDIX|List of Figures|List of Tables):?\s*$""".r

      val part = (content zipWithIndex).dropWhile(l => !startLabels.pattern.matcher(l._1).matches() || l._2 < limit)
                                       .takeWhile(l => !endLabels.pattern.matcher(l._1).matches)
                                       .drop(1)

      val headerLabels = """^(\d+\s+)?(References?|REFERENCES?|Bibliography|BIBLIOGRAPHY|References?\s+and\s+Notes?|References?\s+Cited|REFERENCES?\s+CITED|REFERENCES?\s+AND\s+NOTES?):?\s*(\d+)?$""".r
      val lines = part  .map(_._1)
                        .filterNot(l => headerLabels.pattern.matcher(l).matches)

      mergeReferenceLines(lines)
   }

   /**
     * Merges together the lines belonging to the same reference, and removes the prepended reference label
     * @param lines All the reference lines
     * @return A sequence of references
     */
   def mergeReferenceLines(lines: Seq[String]): Seq[String] =
   {
      /**
        * Builds a sequence of subsequences of elements.
        * A subsequence starts with a element respecting a predicate. All the following elements
        * belong to the same subsequence, until a new starting element is found.
        *
        * Head elements that don't follow a starting element are not member of any subsequences.
        * All the elements following the last starting element belong to the same subsequence than
        * this starting element.
        *
        * @param l   A sequence of elements
        * @tparam A
        * @param p The predicate specifying if an element is a starting element.
        * @return The elements grouped by subsequences having the same starting element.
        */
      def iterativeSpan[A](l: Seq[A], p: (A) => Boolean): Seq[Seq[A]] =
      {
         var remain = l.dropWhile(!p(_)).toList
         var subs = scala.collection.mutable.MutableList[Seq[A]]()

         def nextSubsequence(l: List[A], current: List[A]): (List[A], List[A]) =
         {
            l match{
               case Nil => (current, Nil)
               case head :: tail => if(p(head)) (current, l)
               else nextSubsequence(tail, current :+ head)
            }
         }

         while(!remain.isEmpty){
            val ret = nextSubsequence(remain.tail, List(remain.head))
            val subset = ret._1
            subs += subset.toSeq
            remain = ret._2
         }

         subs.toSeq
      }

      /**
        * Merges several lines by removing trailing '-' at the end of a line
        * This function assumes that a trailing '-' should always be removed as
        * they are the result of an hyphenization.
        * @param lines the lines to merge
        * @return the lines merged into a single line
        */
      def mergeLines(lines: Seq[String]): String =
      {
         val hyphenRegex = """\w-$""".r
         val letterEndRegex = """\w$""".r

         lines.foldLeft("")((res,line) => {
            if(hyphenRegex.findFirstIn(res).isDefined) res.dropRight(1) + line
            else if(letterEndRegex.findFirstIn(res).isDefined) res + " " + line
                 else res + line
         })
      }

      val refLabels = """^((\[[^\]]+\])|(\([^\]]+\)))""".r
      val subseqs = iterativeSpan(lines, (ref: String) =>  refLabels.findFirstIn(ref.trim).isDefined)
      val citations = subseqs.map(mergeLines)
                             .map(line => line.trim.replaceAll(refLabels.regex, "").trim)

      citations
   }

   /**
     * Extract PDF content from an input stream
     * @param stream an inputstream contening informations about a pdf file.
     * @return a textual representation of the pdf content, as a sequence of strings
     */
   def extractContent(stream: InputStream): Seq[String] =
   {
      val parser = new PDFParser(stream)
      //val parser = new PDFParser(url.openStream)
      parser.parse()
      val doc = parser.getDocument();
      val pdfStripper = new PDFTextStripper();
      val pdDoc = new PDDocument(doc);
      val parsedText = pdfStripper.getText(pdDoc);
      pdDoc.close

      val substitutions = Seq(
         ("a¨", "ä"),
         ("u¨", "ü"),
         ("o¨", "ö"),
         ("i¨", "ï"),
         ("e¨", "ë"),
         ("a^", "â"),
         ("u^", "û"),
         ("o^", "ô"),
         ("i^", "î"),
         ("e^", "ê"),
         ("a~", "ã"),
         ("o~", "õ"),
         ("a´", "á"),
         ("u´", "ú"),
         ("o´", "ó"),
         ("i´", "í"),
         ("ı´", "í"),
         ("e´", "é"),
         ("a`", "à"),
         ("u`", "ù"),
         ("o`", "ò"),
         ("i`", "ì"),
         ("e`", "è")

      )

      substitutions.foldLeft(parsedText)((res,next) => res.replaceAll(next._1, next._2))
                   .split('\n')
   }


}
