package com.github.mgoeminne.sitar.parser.acm

import com.github.mgoeminne.sitar.parser.{Citation, CitationParser}

/**
  * ieeetr style for book citation
  */
class ACMBookParser extends CitationParser
{
   def lastName: Parser[String] = """[^,]+""".r ^^ { case l => l.split(" ").last}
   def firstName: Parser[String] = """(\w\.\s?)+""".r

   def author: Parser[String] = lastName ~ "," ~ firstName ^^ { case l~","~f => l}

   def authors: Parser[Seq[String]] =   rep(author ~ ",") ~ "and" ~ author ^^ { case a ~ "and" ~ c => a.map(_._1) :+ c} |
                                        author ~ ", and" ~ author ^^ { case a ~ ", and" ~ b  => Seq(a,b)} |
                                        author ^^ { case a => Seq(a) }

   def title: Parser[String]    = """((?!\.\s).)*""".r
   def rest: Parser[Any]     = """.*""".r
   def citation: Parser[Citation] = authors~opt(""", Eds?\.""".r)~title~"."~rest ^^ { case a~e~t~"."~r => {
      println(a)
      println(e)
      println(t)
      Citation(a, t)
   } }

}
