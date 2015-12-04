package com.github.mgoeminne.sitar.parser

import scala.util.parsing.combinator.JavaTokenParsers

trait CitationParser extends JavaTokenParsers
{
  def citation: Parser[Citation]
}
