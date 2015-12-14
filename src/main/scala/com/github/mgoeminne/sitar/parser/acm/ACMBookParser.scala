package com.github.mgoeminne.sitar.parser.acm

import com.github.mgoeminne.sitar.parser.{Book, Citation, CitationParser}

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
   def editor: Parser[String] = opt(""", Ed(s)?\.""".r) ^^ { case e => e.getOrElse("")}

   def volume: Parser[String] = """,\svol(\.|ume)\s\d+\sof[^\.]+""".r

   def title: Parser[String]    = """[^,\.]+""".r ^^ {case t => println(t) ; t}
   def printer: Parser[String]     = """[^\d]+""".r ^^ { case p => println(p) ; p}
   def year: Parser[Int]         = """\d{4}""".r ^^ {case y => println(y); y.toInt}

   def citation: Parser[Book] = authors~editor~title~opt(volume)~"."~printer~year<~"." ^^ { case a~e~t~v~"."~p~y =>
      new Book(t, a, y)
   }
}