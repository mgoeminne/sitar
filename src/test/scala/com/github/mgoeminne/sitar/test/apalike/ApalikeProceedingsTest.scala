package com.github.mgoeminne.sitar.test.apalike

import com.github.mgoeminne.sitar.parser.Citation
import com.github.mgoeminne.sitar.parser.apalike.parser
import org.scalatest.{FlatSpec, Matchers}

class ApalikeProceedingsTest extends FlatSpec with Matchers
{
   val p = parser.proceedingsParser


   "One author proceedings citation" should "be correctly parsed" in {
      val citation = "Werner, B., editor (2013). Proceedings of the 17th European Conference on Software Maintenance and Reengineering, Genova, Italy. IEEE Computer Society."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "Proceedings of the 17th European Conference on Software Maintenance and Reengineering"
            matched.authors.size shouldBe 1
            matched.authors(0) shouldEqual "Werner"
         }
      }
   }

   "Two authors proceedinds citation" should "be correctly parsed" in {
      val citation = "Litoiu, M. and Mylopoulos, J., editors (2013). Proceedings of the 8th International Symposium on Software Engineering for Adaptive and Self-Managing Systems. SEAMS 2013, San Francisco, CA, USA, May 20-21, 2013. IEEE / ACM."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "Proceedings of the 8th International Symposium on Software Engineering for Adaptive and Self-Managing Systems"
            matched.authors.size shouldBe 2
            matched.authors shouldEqual Seq("Litoiu", "Mylopoulos")
         }

         case p.Failure(msg,_) => fail("Parsing failed : " + msg)
         case p.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }

   "Four authors proceedings citation" should "be correctly parsed" in {
      val citation = "Mens, T., Claes, M., Drobisz, S., and Goeminne, M., editors (2013). BENEVOL 2013 Software Evolution Research Seminar."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "BENEVOL 2013 Software Evolution Research Seminar"
            matched.authors shouldEqual Seq("Mens", "Claes", "Drobisz", "Goeminne")
         }
      }
   }

   "Three authors book citation" should "be correctly parsed" in {
      val citation = "Bloom, B. S., Hastings, J. T., and Madaus, G. F., editors (1971). Handbook on Formative and Summative Evolution of Student Learning. McGraw-Hill, New York."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "Handbook on Formative and Summative Evolution of Student Learning"
            matched.authors shouldEqual Seq("Bloom", "Hastings", "Madaus")
         }
      }
   }


}


