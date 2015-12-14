package com.github.mgoeminne.sitar.parser.ieeetr

import com.github.mgoeminne.sitar.parser.{Book, Citation, ieeetr}
import org.scalatest.{FlatSpec, Matchers}

class IEEETRthesisTest extends FlatSpec with Matchers
{
   val p = parser.thesisParser

   "Single author phd thesis citation" should "be correctly parsed" in {
      val citation = "P. van der Spek, Managing software evolution in embedded systems. PhD thesis, Vrije Universiteit, Belgium, 2010."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Book,_) => {
            matched.title shouldBe "Managing software evolution in embedded systems"
            matched.authors shouldEqual Seq("Spek")
            matched.year shouldBe 2010
         }
      }
   }
}
