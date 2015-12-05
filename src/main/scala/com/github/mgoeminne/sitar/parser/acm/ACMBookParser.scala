package com.github.mgoeminne.sitar.parser.acm

import com.github.mgoeminne.sitar.parser.{Citation, CitationParser}

/**
  * acm style for book citation
  */
private[acm] class ACMBookParser extends CitationParser
{
   def lastName: Parser[String] = """[^,]+""".r ^^ { case l => l.split(" ").last}
   def firstName: Parser[String] = """([A-Z]\.\s?)+""".r

   def author: Parser[String] = lastName ~ "," ~ firstName ^^ { case l~","~f => l}

   def authors: Parser[Seq[String]] =   rep(author ~ ",") ~ "and" ~ author ^^ { case a ~ "and" ~ c => a.map(_._1) :+ c} |
      author ~ ", and" ~ author ^^ { case a ~ ", and" ~ b  => Seq(a,b)} |
      author ^^ { case a => Seq(a) }
   def editor: Parser[String] = opt(""", Ed(s)?\.""".r) ^^ { case e => println(e);e.getOrElse("")}


   def title: Parser[String]    = """((?!\.\s).)*""".r ^^ { case t => t.stripSuffix(", vol")}
   def rest: Parser[Any]     = """.*""".r
   def citation: Parser[Citation] = authors~editor~title~"."~rest ^^ { case a~e~t~"."~r => {
      println(a)
      println(e)
      println(t)
      Citation(a, t)
   } }

}