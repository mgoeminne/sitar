package com.github.mgoeminne.sitar.parser.abbrv

import com.github.mgoeminne.sitar.parser.{Citation, abbrv}
import org.scalatest.{FlatSpec, Matchers}


class ABBRVTechnicalReportTest extends FlatSpec with Matchers
{
   val p = parser.technicalReportParser

   "One author technical report citation" should "be correctly parsed" in {
      val citation = "B. Vanderose. Towards a model-centric quality assessment: the mocqa approach. Technical report, University of Namur, January 2010."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "Towards a model-centric quality assessment: the mocqa approach"
            matched.authors.size shouldBe 1
            matched.authors(0) shouldEqual "Vanderose"
         }

         case p.Failure(msg,_) => fail("Parsing failed : " + msg)
         case p.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }

   "Two authors technical report citation" should "be correctly parsed" in {
      val citation = "P. Lambert and J. Doe. The title of the work. Technical Report 2, The institution that published, The address of the publisher, 7 1993. An optional note."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "The title of the work"
            matched.authors.size shouldBe 2
            matched.authors shouldEqual Seq("Lambert", "Doe")
         }

         case p.Failure(msg,_) => fail("Parsing failed : " + msg)
         case p.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }

   "Three authors technical report citation" should "be correctly parsed" in {
      val citation = "A. K. Jain, L. Hong, and S. Pankanti. Biometrics: Promising frontiers for emerging identification market. Technical Report MSU-CSE-00-2, Department of Computer Science, Michigan State University, East Lansing, Michigan, February 2000."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "Biometrics: Promising frontiers for emerging identification market"
            matched.authors.size shouldBe 3
            matched.authors shouldEqual Seq("Jain", "Hong", "Pankanti")
         }

         case p.Failure(msg,_) => fail("Parsing failed : " + msg)
         case p.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }
}
