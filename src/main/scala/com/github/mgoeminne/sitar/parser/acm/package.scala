package com.github.mgoeminne.sitar.parser

/**
  * Citation parsers for the acm style.
  */
package object acm
{
   val inProceedingsParser = new ACMInProceedingsParser()
   val articleParser = inProceedingsParser
   val bookChapterParser = inProceedingsParser
   val technicalReportParser = inProceedingsParser

   val bookParser = new ACMBookParser()
   val proceedindsParser = new ACMProceedingsParser()

   val thesisParser = inProceedingsParser

}
