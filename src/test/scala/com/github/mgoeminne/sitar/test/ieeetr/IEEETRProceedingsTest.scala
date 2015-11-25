package com.github.mgoeminne.sitar.test.ieeetr

import com.github.mgoeminne.sitar.parser.{Citation, ieeetr}
import com.github.mgoeminne.sitar.test.ieeetr
import org.scalatest.{FlatSpec, Matchers}

class IEEETRProceedingsTest extends FlatSpec with Matchers
{
   val parser = ieeetr.proceedindsParser


   "One author proceedings citation" should "be correctly parsed" in {
      val citation = "B. Werner, ed., Proceedings of the 17th European Conference on Software Maintenance and Reengineering, (Genova, Italy), IEEE Computer Society, March 2013."

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
      val citation = "M. Litoiu and J. Mylopoulos, eds., Proceedings of the 8th International Symposium on Software Engineering for Adaptive and Self-Managing Systems. SEAMS 2013, San Francisco, CA, USA, May 20-21, 2013, IEEE / ACM, 2013."

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
      val citation = "T. Mens, M. Claes, S. Drobisz, and M. Goeminne, eds., BENEVOL 2013 Software Evolution Research Seminar, December 2013."

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


