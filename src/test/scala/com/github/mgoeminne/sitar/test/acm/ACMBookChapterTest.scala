package com.github.mgoeminne.sitar.parser.acm

import com.github.mgoeminne.sitar.parser.{Paper, Book, Citation}
import org.scalatest.{FlatSpec, Matchers}


class ACMBookChapterTest extends FlatSpec with Matchers
{
   val p = parser.bookChapterParser

   "Single author book chapter citation" should "be correctly parsed" in {
      val citation = "Kieren, T. E. Rational and fractional numbers: From quotient fields to recursive understanding. In How Students Learn: History, Mathematics, and Science in the Classroom, S. Donnovan and J. D. Bransford, Eds. The National Academies Press, 2005."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Paper,_) => {
            matched.title shouldBe "Rational and fractional numbers: From quotient fields to recursive understanding"
            matched.authors shouldBe Seq("Kieren")
            matched.year shouldBe 2005
            matched.in shouldBe "How Students Learn: History, Mathematics, and Science in the Classroom"
         }
      }
   }

   "Two authors book chapter citation" should "be correctly parsed" in {
      val citation = "Grégoire, J., and Meerte, G. L’apprentissage des nombres rationnels et ses obstacles. In La dyscalculie Trouble du développement numérique de l’enfant. Noël, Marie-Pascale, décembre 2005."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Paper,_) => {
            matched.title shouldBe "L’apprentissage des nombres rationnels et ses obstacles"
            matched.authors shouldEqual Seq("Grégoire", "Meerte")
            matched.year shouldBe 2005
            matched.in shouldBe "La dyscalculie Trouble du développement numérique de l’enfant"
         }
      }
   }

   "Four authors book chapter citation" should "be correclty parsed" in {
      val citation1 = "Fernandez-Ramil, J., Lozano, A., Wermelinger, M., and Capiluppi, A. Empirical studies of open source evolution. In Software Evolution, T. Mens and S. Demeyer, Eds. 2008, pp. 263–288."

      p.parseAll(p.citation, citation1) match {
         case p.Success(matched: Paper,_) => {
            matched.title shouldBe "Empirical studies of open source evolution"
            matched.authors shouldEqual Seq("Fernandez-Ramil", "Lozano", "Wermelinger", "Capiluppi")
            matched.year shouldBe 2008
            matched.in shouldBe "Software Evolution"
         }
      }
   }
}
