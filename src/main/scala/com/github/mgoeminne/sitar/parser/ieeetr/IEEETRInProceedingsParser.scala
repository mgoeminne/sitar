package com.github.mgoeminne.sitar.parser.ieeetr

import com.github.mgoeminne.sitar.parser.{Paper, Citation, CitationParser}

private[ieeetr] class IEEETRInProceedingsParser extends CitationParser
{
  def author: Parser[Seq[String]]   = """(?!and)[^,“]+(?!and)""".r ^^ {case s => s.split(" and ").toList.map(_.trim.split("""\s""").last.trim)}
  def authors: Parser[Seq[String]]  = (rep(author~",")~"and"~author) ^^ {case l~"and"~e => l.map(_._1).flatten ++ e} |
                                      author ^^ { case s => s }

  def title: Parser[String]    = """[^”]+""".r ^^ {case t => t.stripSuffix(",")}
  def rest: Parser[Any]     = """.*""".r
  def citation: Parser[Paper] = authors~","~"“"~title~"”"~rest ^^ { case a~","~"“"~t~"”"~r => new Paper(t, a, 42, "youhou") }
}