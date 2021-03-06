package com.github.mgoeminne.sitar.parser.acm

import com.github.mgoeminne.sitar.parser.{Paper, Citation, CitationParser}

/**
  * acm style for inproceedings citation
  */
private[acm] class ACMInProceedingsParser extends CitationParser
{
  def lastName: Parser[String] = """[^,]+""".r ^^ { case l => l.split(" ").last}
  def firstName: Parser[String] = """(\p{Lu}\.\s?)+""".r

  def author: Parser[String] = lastName ~ "," ~ firstName ^^ { case l~","~f => l}

  def authors: Parser[Seq[String]] =   rep(author ~ ",") ~ "and" ~ author ^^ { case a ~ "and" ~ c => a.map(_._1) :+ c} |
                                       author ~ ", and" ~ author ^^ { case a ~ ", and" ~ b => Seq(a,b)} |
                                       author ^^ { case a => Seq(a) }

  def title: Parser[String]    = """((?!\.\s).)*""".r
  def rest: Parser[Any]     = """.*""".r
  def citation: Parser[Paper] = authors~title~"."~rest ^^ { case a~t~"."~r => new Paper(t, a, 42, "youhou") }
}


