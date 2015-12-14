package com.github.mgoeminne.sitar.parser.acm

import com.github.mgoeminne.sitar.parser.{Citation}
import org.scalatest.{FlatSpec, Matchers}

class ACMthesisTest extends FlatSpec with Matchers
{
   val p = parser.thesisParser

   "Single author phd thesis citation" should "be correctly parsed" in {
      val citation = "van der Spek, P. Managing software evolution in embedded systems. PhD thesis, Vrije Universiteit, Belgium, 2010."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "Managing software evolution in embedded systems"
            matched.authors shouldEqual Seq("Spek")
            matched.year shouldEqual 2010
         }

         case p.Failure(msg,_) => fail("Parsing failed : " + msg)
         case p.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }
}
