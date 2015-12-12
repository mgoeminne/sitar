package com.github.mgoeminne.sitar.parser.apalike

import com.github.mgoeminne.sitar.parser.StyleParser
import com.github.mgoeminne.sitar.parser.ieeetr.{IEEETRThesisParser, IEEETRBookParser, IEEETRInProceedingsParser}


object parser extends StyleParser
{
   val inProceedingsParser = new ApalikeInProceedingsParser()
   val articleParser = inProceedingsParser
   val bookChapterParser = inProceedingsParser
   val technicalReportParser = inProceedingsParser
   val thesisParser = inProceedingsParser

   val bookParser = new ApalikeBookParser()
   val proceedingsParser = bookParser

   override def toString = "apalike"
}
