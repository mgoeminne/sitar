package com.github.mgoeminne.sitar.parser.abbrv

/**
  * Citation parsers for the abbrv style.
  */
object parser
{
   val inProceedingsParser = new ABBRVInProceedingsParser()
   val technicalReportParser = inProceedingsParser

   val articleParser = new ABBRVArticleParser()
   val bookChapterParser = articleParser
   val thesisParser = articleParser

   val bookParser = new ABBRVBookParser()
   val proceedindsParser = bookParser
}
