package com.github.mgoeminne.sitar.parser.apalike

import com.github.mgoeminne.sitar.parser.{Book, Citation, CitationParser}

/**
  * apalike style for book citation
  */
private[apalike] class ApalikeBookParser extends CitationParser
{
   def lastName: Parser[String] = """[^,]+""".r ^^ { case l => l.split(" ").last}
   def firstName: Parser[String] = """([A-Z]\. )*[A-Z]\.""".r

   def year: Parser[Int] = """\(\d+\)\.""".r ^^ { case y => y.drop(1).dropRight(2).toInt}
   def volume: Parser[String] = """, volume[^\.]+""".r

   def author: Parser[String] = lastName ~ "," ~ firstName ^^ { case l~","~f => println(l); l}

   def authors: Parser[Seq[String]] =   rep(author ~ ",") ~ "and" ~ author ^^ { case a ~ "and" ~ c => println("!" + a); a.map(_._1) :+ c} |
                                          author ~ "and" ~ author ^^ { case a ~ "and" ~ b  => println("!!" + a); Seq(a,b)} |
                                          author ^^ { case a => println("!!!" + a) ; Seq(a) }
   def editor: Parser[String] =   """, editor(s)?""".r  |
                                  opt(""", ed(s)?""".r) ^^ { case e => e.getOrElse("")}


   def title = """[^\.,]+""".r ^^ { case t => println(t); t}

   def rest: Parser[Any]     = """.*""".r
   def citation: Parser[Book] = authors~editor~year~title~rest ^^ { case a~e~y~t~r => {
      new Book(t, a, 42)
   } }
}