package com.github.mgoeminne.sitar.parser.ieeetr

import com.github.mgoeminne.sitar.parser.StyleParser

/**
  * Citation parsers for the ieeetr style.
  */
object parser extends StyleParser
{
   val inProceedingsParser = new IEEETRInProceedingsParser()
   val articleParser = inProceedingsParser
   val bookChapterParser = inProceedingsParser
   val technicalReportParser = inProceedingsParser

   val bookParser = new IEEETRBookParser()
   val proceedingsParser = bookParser

   val thesisParser = new IEEETRThesisParser()

   override def toString = "ieeetr"
}

