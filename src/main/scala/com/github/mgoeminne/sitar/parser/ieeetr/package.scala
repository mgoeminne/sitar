package com.github.mgoeminne.sitar.parser

/**
  * Citation parsers for the ieeetr style.
  */
package object ieeetr
{
   val inProceedingsParser = new IEEETRInProceedingsParser()
   val articleParser = inProceedingsParser
   val bookChapterParser = inProceedingsParser
   val technicalReportParser = inProceedingsParser

   val bookParser = new IEEETRBookParser()
   val proceedindsParser = bookParser

   val thesisParser = new IEEETRThesisParser()

}
