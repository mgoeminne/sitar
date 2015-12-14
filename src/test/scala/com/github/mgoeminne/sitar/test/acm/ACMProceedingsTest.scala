package com.github.mgoeminne.sitar.parser.acm

import com.github.mgoeminne.sitar.parser.{Book, Citation}
import org.scalatest.{FlatSpec, Matchers}

class ACMProceedingsTest extends FlatSpec with Matchers
{
   val p = parser.proceedingsParser


   "One author proceedings citation" should "be correctly parsed" in {
      val citation = "Werner, B., Ed. Proceedings of the 17th European Conference on Software Maintenance and Reengineering (Genova, Italy, March 2013), IEEE Computer Society."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Book,_) => {
            matched.authors shouldBe Seq("Werner")
            matched.title shouldBe "Proceedings of the 17th European Conference on Software Maintenance and Reengineering"
            matched.year shouldBe 2013
         }
      }
   }

   "Two authors proceedinds citation" should "be correctly parsed" in {
      val citation = "Litoiu, M., and Mylopoulos, J., Eds. Proceedings of the 8th International Symposium on Software Engineering for Adaptive and Self-Managing Systems. SEAMS 2013, San Francisco, CA, USA, May 20-21, 2013 (2013), IEEE / ACM."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Book,_) => {
            matched.title shouldBe "Proceedings of the 8th International Symposium on Software Engineering for Adaptive and Self-Managing Systems"
            matched.authors shouldEqual Seq("Litoiu", "Mylopoulos")
            matched.year shouldBe 2013
         }
      }
   }

   "Four authors proceedings citation" should "be correctly parsed" in {
      val citation = "Mens, T., Claes, M., Drobisz, S., and Goeminne, M., Eds. BENEVOL 2013 Software Evolution Research Seminar (December 2013)."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.authors shouldEqual Seq("Mens", "Claes", "Drobisz", "Goeminne")
            matched.title shouldBe "BENEVOL 2013 Software Evolution Research Seminar"
            matched.year shouldBe 2013
         }
      }
   }

}


