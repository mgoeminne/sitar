package com.github.mgoeminne.sitar.parser.apalike

import com.github.mgoeminne.sitar.parser.{Paper, Citation, CitationParser}

/**
  * apalike style for inproceedings citation
  */
private[apalike] class ApalikeInProceedingsParser extends CitationParser
{
  def year: Parser[Int] = """\(\d+\)\.""".r ^^ { case y => y.drop(1).dropRight(2).toInt}
  def lastName: Parser[String] = """[^,]+""".r ^^ { case l => l.split(" ").last}
  def firstName: Parser[String] = """(\p{Lu}\.\s?)+""".r

  def author: Parser[String] = lastName ~ "," ~ firstName ^^ { case l~","~f => l}

  def authors: Parser[Seq[String]] =   rep(author ~ ",") ~ "and" ~ author ^^ { case a ~ "and" ~ c => a.map(_._1) :+ c} |
                                       author ~ "and" ~ author ^^ { case a ~ "and" ~ b => Seq(a,b)} |
                                       author ^^ { case a => Seq(a) }

  def title: Parser[String]    = """[^\.]+""".r
  def rest: Parser[Any]     = """.*""".r
  def citation: Parser[Paper] = authors~year~title~"."~rest ^^ { case a~y~t~"."~r => new Paper(t, a, 42, "youhou") }
}


