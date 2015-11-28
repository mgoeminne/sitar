package com.github.mgoeminne.sitar.parser.ieeetr

import com.github.mgoeminne.sitar.parser.{Citation, CitationParser}

class IEEETRThesisParser extends CitationParser
{
   def author: Parser[String]   = """[^,]+""".r ^^ {case s => s.split(" ").last.trim}

   def title: Parser[String]     = """.*""".r ^^ {case t => t.split("PhD thesis").head.trim.stripSuffix(".")}
   def citation: Parser[Citation] = author~","~title ^^ { case a~","~t => Citation(Seq(a), t) }
}
