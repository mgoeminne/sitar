package com.github.mgoeminne.sitar.parser.acm

import com.github.mgoeminne.sitar.parser.{Citation, CitationParser}

/**
  * ieeetr style for book citation
  */
class ACMProceedingsParser extends CitationParser
{
   def lastName: Parser[String] = """[^,]+""".r ^^ { case l => l.split(" ").last}
   def firstName: Parser[String] = """([A-Z]\.\s?)+""".r

   def author: Parser[String] = lastName ~ "," ~ firstName ^^ { case l~","~f => l}

   def authors: Parser[Seq[String]] =   rep1sep(author, ",") |
                                        author ~ ", and" ~ author ^^ { case a ~ ", and" ~ b  => Seq(a,b)} |
                                        author ^^ { case a => println(a); Seq(a) }
   def editor: Parser[String] = opt(""", Ed(s)?\.""".r) ^^ { case e => println(e);e.getOrElse("")}


   def title: Parser[String]    = """[^\.\(]+""".r ^^ {case t => t.trim}
   def rest: Parser[Any]        = """.*""".r
   def citation: Parser[Citation] = authors~editor~title~rest ^^ { case a~e~t~r => Citation(a, t) }

}