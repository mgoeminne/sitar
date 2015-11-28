package com.github.mgoeminne.sitar.test.abbrv

import com.github.mgoeminne.sitar.parser.{Citation, abbrv}
import org.scalatest.{FlatSpec, Matchers}

class ABBRVthesisTest extends FlatSpec with Matchers
{
   val parser = abbrv.thesisParser

   "Single author phd thesis citation" should "be correctly parsed" in {
      val citation = "P. van der Spek. Managing software evolution in embedded systems. PhD thesis, Vrije Universiteit, Belgium, 2010."

      parser.parseAll(parser.citation, citation) match {
         case parser.Success(matched: Citation,_) => {
            matched.title shouldBe "Managing software evolution in embedded systems"
            matched.authors.size shouldBe 1
            matched.authors(0) shouldBe "Spek"
         }
      }
   }
}
