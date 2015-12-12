package com.github.mgoeminne.sitar.test.apalike

import com.github.mgoeminne.sitar.parser.Citation
import com.github.mgoeminne.sitar.parser.apalike.parser
import org.scalatest.{FlatSpec, Matchers}


class ApalikeBookChapterTest  extends FlatSpec with Matchers
{
   val p = parser.bookChapterParser

   "Single author book chapter citation" should "be correctly parsed" in {
      val citation = "Kieren, T. E. (2005). Rational and fractional numbers: From quotient fields to recursive understanding. In Donnovan, S. and Bransford, J. D., editors, How Students Learn: History, Mathematics, and Science in the Classroom. The National Academies Press."

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
      val citation = "Grégoire, J. and Meerte, G. (2005). L’apprentissage des nombres rationnels et ses obstacles. In La dyscalculie Trouble du développement numérique de l’enfant. Noël, Marie-Pascale."

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
      val citation = "Fernandez-Ramil, J., Lozano, A., Wermelinger, M., and Capiluppi, A. (2008). Empirical studies of open source evolution. In Mens, T. and Demeyer, S., editors, Software Evolution. Springer."

      p.parseAll(p.citation, citation) match {
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
