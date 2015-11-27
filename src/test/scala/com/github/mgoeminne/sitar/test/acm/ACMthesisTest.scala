package com.github.mgoeminne.sitar.test.acm

import com.github.mgoeminne.sitar.parser.{Citation, acm}
import org.scalatest.{FlatSpec, Matchers}

class ACMthesisTest extends FlatSpec with Matchers
{
   val parser = acm.thesisParser

   "Single author phd thesis citation" should "be correctly parsed" in {
      val citation = "van der Spek, P. Managing software evolution in embedded systems. PhD thesis, Vrije Universiteit, Belgium, 2010."

      parser.parseAll(parser.citation, citation) match {
         case parser.Success(matched: Citation,_) => {
            matched.title shouldBe "Managing software evolution in embedded systems"
            matched.authors.size shouldBe 1
            matched.authors(0) shouldBe "Spek"
         }

         case parser.Failure(msg,_) => fail("Parsing failed : " + msg)
         case parser.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }
}
