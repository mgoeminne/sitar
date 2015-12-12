package com.github.mgoeminne.sitar.test.apalike

import com.github.mgoeminne.sitar.parser.Citation
import com.github.mgoeminne.sitar.parser.apalike.parser
import org.scalatest.{FlatSpec, Matchers}

class ApalikeThesisTest extends FlatSpec with Matchers
{
   val p = parser.thesisParser

   "Single author phd thesis citation" should "be correctly parsed" in {
      val citation = "van der Spek, P. (2010). Managing software evolution in embedded systems. PhD thesis, Vrije Universiteit, Belgium."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "Managing software evolution in embedded systems"
            matched.authors.size shouldBe 1
            matched.authors(0) shouldBe "Spek"
         }

         case p.Failure(msg,_) => fail("Parsing failed : " + msg)
         case p.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }
}
