package com.github.mgoeminne.sitar.parser.acm

import com.github.mgoeminne.sitar.parser.{Book, Paper, Citation}
import org.scalatest.{FlatSpec, Matchers}


class ACMTechnicalReportTest extends FlatSpec with Matchers
{
   val p = parser.technicalReportParser

   "One author technical report citation" should "be correctly parsed" in {
      val citation = "Vanderose, B. Towards a model-centric quality assessment: the mocqa approach. Tech. rep., University of Namur, January 2010."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "Towards a model-centric quality assessment: the mocqa approach"
            matched.authors shouldEqual Seq("Vanderose")
            matched.year shouldEqual 2010
         }
      }
   }

   "Two authors technical report citation" should "be correctly parsed" in {
      val citation = "Lambert, P., and Doe, J. The title of the work. Tech. Rep. 2, The institution that published, The address of the publisher, 7 1993. An optional note."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "The title of the work"
            matched.authors shouldEqual Seq("Lambert", "Doe")
            matched.year shouldEqual 1993
         }
      }
   }

   "Three authors technical report citation" should "be correctly parsed" in {
      val citation = "Jain, A. K., Hong, L., and Pankanti, S. Biometrics: Promising frontiers for emerging identification market. Tech. Rep. MSU-CSE-00-2, Department of Computer Science, Michigan State University, East Lansing, Michigan, February 2000."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "Biometrics: Promising frontiers for emerging identification market"
            matched.authors shouldEqual Seq("Jain", "Hong", "Pankanti")
            matched.year shouldEqual 2000
         }
      }
   }
}
