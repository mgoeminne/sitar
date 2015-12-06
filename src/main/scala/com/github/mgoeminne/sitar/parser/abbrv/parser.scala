package com.github.mgoeminne.sitar.parser.abbrv

import com.github.mgoeminne.sitar.parser.StyleParser

/**
  * Citation parsers for the abbrv style.
  */
object parser extends StyleParser
{
   val inProceedingsParser = new ABBRVInProceedingsParser()
   val technicalReportParser = inProceedingsParser

   val articleParser = new ABBRVArticleParser()
   val bookChapterParser = articleParser
   val thesisParser = articleParser

   val bookParser = new ABBRVBookParser()
   val proceedingsParser = bookParser

   override def toString = "abbrv"
}
