package com.github.mgoeminne.sitar.test.acm

import com.github.mgoeminne.sitar.parser.{Citation, acm}
import org.scalatest.{FlatSpec, Matchers}

class ACMProceedingsTest extends FlatSpec with Matchers
{
   val parser = acm.proceedindsParser


   "One author proceedings citation" should "be correctly parsed" in {
      val citation = "Werner, B., Ed. Proceedings of the 17th European Conference on Software Maintenance and Reengi- neering (Genova, Italy, March 2013), IEEE Computer Society."

      parser.parseAll(parser.citation, citation) match {
         case parser.Success(matched: Citation,_) => {
            matched.title shouldBe "Proceedings of the 17th European Conference on Software Maintenance and Reengineering"
            matched.authors.size shouldBe 1
            matched.authors(0) shouldEqual "Werner"
         }

         case parser.Failure(msg,_) => fail("Parsing failed : " + msg)
         case parser.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }

   "Two authors proceedinds citation" should "be correctly parsed" in {
      val citation = "Litoiu, M., and Mylopoulos, J., Eds. Proceedings of the 8th International Symposium on Software Engineering for Adaptive and Self-Managing Systems. SEAMS 2013, San Francisco, CA, USA, May 20-21, 2013 (2013), IEEE / ACM."

      parser.parseAll(parser.citation, citation) match {
         case parser.Success(matched: Citation,_) => {
            matched.title shouldBe "Proceedings of the 8th International Symposium on Software Engineering for Adaptive and Self-Managing Systems"
            matched.authors.size shouldBe 2
            matched.authors shouldEqual Seq("Litoiu", "Mylopoulos")
         }

         case parser.Failure(msg,_) => fail("Parsing failed : " + msg)
         case parser.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }

   "Four authors proceedings citation" should "be correctly parsed" in {
      val citation = "Mens, T., Claes, M., Drobisz, S., and Goeminne, M., Eds. BENEVOL 2013 Software Evolution Research Seminar (December 2013)."

      parser.parseAll(parser.citation, citation) match {
         case parser.Success(matched: Citation,_) => {
            matched.title shouldBe "BENEVOL 2013 Software Evolution Research Seminar"
            matched.authors.size shouldBe 4
            matched.authors shouldEqual Seq("Mens", "Claes", "Drobisz", "Goeminne")
         }

         case parser.Failure(msg,_) => fail("Parsing failed : " + msg)
         case parser.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }

}


