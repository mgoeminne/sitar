package com.github.mgoeminne.sitar.parser.abbrv

import com.github.mgoeminne.sitar.parser.{Citation, CitationParser}

private[abbrv] class ABBRVArticleParser extends CitationParser
{
   def author: Parser[String] = """\p{Lu}\w+(\.|\s|,)""".r

   def authors: Parser[Seq[String]] =   rep(author ~ ",") ~ "and" ~ author ^^ { case a ~ "and" ~ c => a.map(_._1) :+ c} |
                                        author ~ ", and" ~ author ^^ { case a ~ ", and" ~ b  => Seq(a,b)} |
                                        author ^^ { case a => Seq(a) }

   def title: Parser[String]    = """[^\.]+""".r
   def rest: Parser[Any]     = """.*""".r
   def citation: Parser[Citation] = authors~"."~title~rest ^^ { case a~"."~t~r => Citation(a, t) }

}
