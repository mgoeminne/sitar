package com.github.mgoeminne.sitar.parser.abbrv

import com.github.mgoeminne.sitar.parser.{CitationParser, Citation}

class ABBRVInProceedingsParser extends CitationParser
{
   def firstname = """[^\.]""".r  ^^ {case f => f}
   def lastname  = """[^\.,\s]+""".r ^^ {case l => println(l) ; l}

   def author: Parser[String]   = firstname ~ "." ~ lastname ^^ {case f~"."~l => l.split(" ").last}

   def authors: Parser[Seq[String]]  =    rep1sep(author, ",") ~ "and" ~ author ^^ {case a ~ "and" ~ b => a :+ b} |
                                          author ~ "and" ~ author ^^ {case a ~ "and" ~ b => Seq(a,b)} |
                                          author ^^ { case s => Seq(s) }

   def title: Parser[String]    = """[^.]+""".r
   def rest: Parser[Any]     = """.*""".r
   def citation: Parser[Citation] = authors~"."~title~"."~rest ^^ { case a~"."~t~"."~r => Citation(a, t) }
}
