package com.github.mgoeminne.sitar.parser.abbrv

import com.github.mgoeminne.sitar.parser.{Citation, abbrv}
import org.scalatest.{FlatSpec, Matchers}


class ABBRVBookChapterTest extends FlatSpec with Matchers
{
   val p = parser.bookChapterParser

   "Single author book chapter citation" should "be correctly parsed" in {
      val citation = "T. E. Kieren. Rational and fractional numbers: From quotient fields to recursive understanding. In S. Donnovan and J. D. Bransford, editors, How Students Learn: History, Mathematics, and Science in the Classroom. The National Academies Press, 2005."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "Rational and fractional numbers: From quotient fields to recursive understanding"
            matched.authors.size shouldBe 1
            matched.authors(0) shouldBe "Kieren"
         }

         case p.Failure(msg,_) => fail("Parsing failed : " + msg)
         case p.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }

   "Two authors book chapter citation" should "be correctly parsed" in {
      val citation = "J. Grégoire and G. Meerte. L’apprentissage des nombres rationnels et ses obstacles. In La dyscalculie Trouble du déeveloppement numéerique de l’enfant. Noël, Marie-Pascale, décembre 2005."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "L’apprentissage des nombres rationnels et ses obstacles"
            matched.authors.size shouldBe 2
            matched.authors shouldEqual Seq("Grégoire", "Meerte")
         }

         case p.Failure(msg,_) => fail("Parsing failed : " + msg)
         case p.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }

   "Four authors book chapter citation" should "be correclty parsed" in {
      val citation1 = "J. Fernandez-Ramil, A. Lozano, M. Wermelinger, and A. Capiluppi. Empirical studies of open source evolution. In T. Mens and S. Demeyer, editors, Software Evolution, pages 263–288. 2008."

      p.parseAll(p.citation, citation1) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "Empirical studies of open source evolution"
            matched.authors.size shouldBe 4
            matched.authors shouldEqual Seq("Fernandez-Ramil", "Lozano", "Wermelinger", "Capiluppi")
         }

         case p.Failure(msg,_) => fail("Parsing failed : " + msg)
         case p.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }
}
