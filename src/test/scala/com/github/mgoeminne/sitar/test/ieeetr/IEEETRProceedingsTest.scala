package com.github.mgoeminne.sitar.parser.ieeetr

import com.github.mgoeminne.sitar.parser.{Book, Citation, ieeetr}
import org.scalatest.{FlatSpec, Matchers}

class IEEETRProceedingsTest extends FlatSpec with Matchers
{
   val p = parser.proceedingsParser


   "One author proceedings citation" should "be correctly parsed" in {
      val citation = "B. Werner, ed., Proceedings of the 17th European Conference on Software Maintenance and Reengineering, (Genova, Italy), IEEE Computer Society, March 2013."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Book,_) => {
            matched.title shouldBe "Proceedings of the 17th European Conference on Software Maintenance and Reengineering"
            matched.authors shouldEqual Seq("Werner")
            matched.year shouldBe 2013
         }
      }
   }

   "Two authors proceedinds citation" should "be correctly parsed" in {
      val citation = "M. Litoiu and J. Mylopoulos, eds., Proceedings of the 8th International Symposium on Software Engineering for Adaptive and Self-Managing Systems. SEAMS 2013, San Francisco, CA, USA, May 20-21, 2013, IEEE / ACM, 2013."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Book,_) => {
            matched.title shouldBe "Proceedings of the 8th International Symposium on Software Engineering for Adaptive and Self-Managing Systems"
            matched.authors shouldEqual Seq("Litoiu", "Mylopoulos")
            matched.year shouldBe 2013
         }
      }
   }

   "Four authors proceedings citation" should "be correctly parsed" in {
      val citation = "T. Mens, M. Claes, S. Drobisz, and M. Goeminne, eds., BENEVOL 2013 Software Evolution Research Seminar, December 2013."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "BENEVOL 2013 Software Evolution Research Seminar"
            matched.authors shouldEqual Seq("Mens", "Claes", "Drobisz", "Goeminne")
            matched.year shouldBe 2013
         }
      }
   }
}


