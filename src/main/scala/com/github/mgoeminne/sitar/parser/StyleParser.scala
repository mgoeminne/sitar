package com.github.mgoeminne.sitar.parser

import com.github.mgoeminne.sitar.parser.acm.{ACMProceedingsParser, ACMBookParser, ACMInProceedingsParser}

/**
  * Defines a parser of a particular style of citation.
  */
trait StyleParser
{
   def inProceedingsParser    : CitationParser
   def articleParser          : CitationParser
   def bookChapterParser      : CitationParser
   def technicalReportParser  : CitationParser

   def bookParser             : CitationParser
   def proceedindsParser      : CitationParser

   def thesisParser           : CitationParser

   private def parsers: Seq[CitationParser] = Seq( inProceedingsParser, articleParser,
                                                   bookChapterParser, technicalReportParser,
                                                   bookParser, proceedindsParser)

   /**
     * Tries to parse a citation using a parser for a particular style of citation.
     * @param citation  The potential citation string
     * @return          The extracted citation, if any
     */
   def parse(citation: String): Option[Citation] =
   {
      parsers.foldLeft(None.asInstanceOf[Option[Citation]])((previous, p) => previous match {
         case s: Some[Citation] => s
         case _ => p.parseAll(p.citation, citation) match {
            case p.Success(result: Citation, _) => Some(result)
            case _ => None
         }
      })
   }
}
