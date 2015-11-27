package com.github.mgoeminne.sitar.parser.acm

import com.github.mgoeminne.sitar.parser.{Citation, CitationParser}

/**
  * ieeetr style for book citation
  */
class ACMBookParser extends CitationParser
{
   def author: Parser[Seq[String]]   = """[^,]+""".r ^^ {case s => s.split(" and ").toList.map(_.trim.split("""\s""").head.trim)}

   def authors: Parser[Seq[String]]  = (rep(author~",")~"and"~author) ^^ {case l~"and"~e => l.map(_._1).flatten ++ e} |
                                       author ^^ { case s => println(s); s }

   def title: Parser[String]    = """[^,]+""".r ^^ {case t => t.replaceAll("""\s+""", " ").replaceAll(""",\svol\s\d+.*""", "")}
   def rest: Parser[Any]     = """.*""".r

   def citation: Parser[Citation] = authors~""", ed(s)?\.,""".r~title~"."~rest ^^ { case a~s~t~"."~r => Citation(a, t) }

}
