package com.github.mgoeminne.sitar.parser

/**
  * Defines a parser of a particular style of citation.
  */
trait StyleParser
{
   /**
     * @return a citation parser for inproceedings citations
     */
   def inProceedingsParser    : CitationParser

   /**
     * @return a citation parser for article citations
     */
   def articleParser          : CitationParser

   /**
     * @return a citation parser for book chapter citations
     */
   def bookChapterParser      : CitationParser

   /**
     * @return a citation parser for technical report citations
     */
   def technicalReportParser  : CitationParser

   /**
     * @return a citation parser for book citations
     */
   def bookParser             : CitationParser

   /**
     * @return a citation parser for proceedinds citations
     */
   def proceedingsParser      : CitationParser

   /**
     * @return a citation parser for thesis citations
     */
   def thesisParser           : CitationParser

   /**
     * @return a sequence of all parsers
     */
   private def parsers: Seq[CitationParser] = Seq( inProceedingsParser, articleParser,
                                                   bookChapterParser, technicalReportParser,
                                                   bookParser, proceedingsParser)

   /**
     * Tries to parse a citation using a parser for a particular style of citation.
     * @param line      The potential citation string.
     * @return          The extracted citation, if any.
     */
   def parse(line: String): Option[Citation] =
   {
      parsers.foldLeft(Option.empty[Citation])((previous, p) => previous match {
         case s: Some[Citation] => s
         case _ => p.parseAll(p.citation, line) match {
            case p.Success(result: Citation, _) => Some(result)
            case _ => None
         }
      })
   }
}
