package com.github.mgoeminne.sitar.parser.ieeetr

import com.github.mgoeminne.sitar.parser.{Book, Citation, CitationParser}

private[ieeetr] class IEEETRThesisParser extends CitationParser
{
   def author: Parser[String]   = """[^,]+""".r ^^ {case s => s.split(" ").last.trim}

   def title: Parser[String]     = """.*""".r ^^ {case t => t.split("PhD thesis").head.trim.stripSuffix(".")}
   def citation: Parser[Book] = author~","~title ^^ { case a~","~t => new Book(t, Seq(a), 42) }
}
