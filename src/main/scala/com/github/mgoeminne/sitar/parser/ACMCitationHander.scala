package com.github.mgoeminne.sitar.parser

/**
 * Parser for ACM citations
 */
class ACMCitationHander extends CitationParser {
  override def citation: Parser[Citation] = ".*" ^^ { case x => Citation(Seq("a"), "b")}
}
