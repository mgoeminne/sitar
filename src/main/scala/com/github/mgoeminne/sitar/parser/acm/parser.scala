package com.github.mgoeminne.sitar.parser.acm

import com.github.mgoeminne.sitar.parser.StyleParser


/**
  * Citation parsers for the acm style.
  */
object parser extends StyleParser
{
   val inProceedingsParser = new ACMInProceedingsParser()
   val articleParser = inProceedingsParser
   val bookChapterParser = inProceedingsParser
   val technicalReportParser = inProceedingsParser
   val thesisParser = inProceedingsParser

   val bookParser = new ACMBookParser()

   val proceedindsParser = new ACMProceedingsParser()
}


