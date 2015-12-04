package com.github.mgoeminne.sitar.parser

/**
  * Citation parsers for the abbrv style.
  */
package object abbrv
{
   val inProceedingsParser = new ABBRVInProceedingsParser()
   val technicalReportParser = inProceedingsParser

   val articleParser = new ABBRVArticleParser()
   val bookChapterParser = articleParser
   val thesisParser = articleParser

   val bookParser = new ABBRVBookParser()
   val proceedindsParser = bookParser
}
