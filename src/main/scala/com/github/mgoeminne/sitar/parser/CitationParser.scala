package com.github.mgoeminne.sitar.parser

import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Generic citation parser
 */
trait CitationParser extends JavaTokenParsers
{
  def citation: Parser[Citation]
}
