package com.github.mgoeminne.sitar.parser

import com.github.mgoeminne.sitar.parser.ieeetr.{IEEETRThesisParser, IEEETRBookParser, IEEETRInProceedingsParser}


package object acm
{
   val inProceedingsParser = new ACMInProceedingsParser()
   val articleParser = inProceedingsParser
   val bookChapterParser = inProceedingsParser
   val technicalReportParser = inProceedingsParser

   val bookParser = new ACMBookParser()
   val proceedindsParser = bookParser

   val thesisParser = new ACMThesisParser()

}
