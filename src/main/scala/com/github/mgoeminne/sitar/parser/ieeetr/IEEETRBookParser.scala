package com.github.mgoeminne.sitar.parser.ieeetr

import com.github.mgoeminne.sitar.parser.{Book, Citation, CitationParser}

/**
  * ieeetr style for book citation
  */
private[ieeetr] class IEEETRBookParser extends CitationParser
{
   def lastname: Parser[String] = """\p{Lu}\w*""".r
   def firstname: Parser[String] = rep("""\p{Lu}\.""".r) ^^ { case f => f.mkString(" ")}
   def editor: Parser[String] = "," ~ opt("""ed(s)?\.,\s+""".r) ^^ { case "," ~ e => e.getOrElse("")}

   def author: Parser[String]   = firstname ~ lastname ^^ {case f~l => l}

   def authors: Parser[Seq[String]]  = (rep(author~",")~"and"~author) ^^ {case a~"and"~b => a.map(_._1) :+ b} |
                                       author~"and"~author ^^ {case a~"and"~b => Seq(a,b)} |
                                       author ^^ { case s =>Seq(s) }

   def title: Parser[String]    = """[^.,]+""".r ^^ {case t => t.replaceAll("""\s+""", " ").stripSuffix(""", vol""")}
   def rest: Parser[Any]            = """.*""".r

   def citation: Parser[Book] = authors~editor~title~""",|\.""".r~rest ^^ { case a~e~t~sep~r => new Book(t, a, 42) }

}
