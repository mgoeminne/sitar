package com.github.mgoeminne.sitar.parser

class CitationParser2 extends CitationParser {

  def last_name: Parser[String]     = """[^\s,:]+""".r
  def first_name: Parser[String]    = """(\w\.)+""".r
  def author: Parser[String]        = last_name ~ "," ~ first_name ^^ { case l ~ "," ~ f => l} |
                                      last_name
  def authors: Parser[Seq[String]]  = rep(author~",") ~ author ^^ { case a ~ b => a.map(_._1) :+ b} |
                                      author ^^ { case x => Seq(x)}
  def title: Parser[String]         = """[^\.]+""".r
  def rest: Parser[String]          = """.*""".r

  def citation: Parser[Citation] = authors ~ ":" ~ title ~ "." ~ rest ^^ { case a~":"~t~"."~r => Citation(a, t)}
}
