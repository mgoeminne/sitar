package com.github.mgoeminne.sitar.parser.acm

import com.github.mgoeminne.sitar.parser.{Citation, CitationParser}

class ACMThesisParser extends CitationParser
{
   def author: Parser[String]   = """[^,]+""".r ^^ {case s => s.split(" ").head.trim}

   def title: Parser[String]     = """.*""".r ^^ {case t => t.split("PhD thesis").head.trim.stripSuffix(".")}
   def citation: Parser[Citation] = author~","~title ^^ { case a~","~t => Citation(Seq(a), t) }
}
