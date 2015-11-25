package com.github.mgoeminne.sitar.test.ieeetr

import com.github.mgoeminne.sitar.parser.{Citation, ieeetr}
import org.scalatest.{FlatSpec, Matchers}


class IEEETRArticleTest extends FlatSpec with Matchers
{
   val parser = ieeetr.articleParser

   "Single author article citation" should "be correctly parsed" in {
      val citation = "Z. Sidak, “Rectangular confidence regions for the means of multivariate normal distributions,” Journal of the American Statistical Association, vol. 62, no. 318, pp. pp. 626–633, 1967."

      parser.parseAll(parser.citation, citation) match {
         case parser.Success(matched: Citation,_) => {
            matched.title shouldBe "Rectangular confidence regions for the means of multivariate normal distributions"
            matched.authors.size shouldBe 1
            matched.authors(0) shouldBe "Sidak"
         }

         case parser.Failure(msg,_) => fail("Parsing failed : " + msg)
         case parser.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }

   "Two authors article citation" should "be correctly parsed" in {
      val citation = "E. L. Kaplan and P. Meier, “Nonparametric estimation from incomplete observations,” Journal of the American Statistical Association, vol. 53, no. 282, pp. pp. 457–481, 1958."

      parser.parseAll(parser.citation, citation) match {
         case parser.Success(matched: Citation,_) => {
            matched.title shouldBe "Nonparametric estimation from incomplete observations"
            matched.authors.size shouldBe 2
            matched.authors shouldEqual Seq("Kaplan", "Meier")
         }

         case parser.Failure(msg,_) => fail("Parsing failed : " + msg)
         case parser.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }

   "Three authors article citation" should "be correclty parsed" in {
      val citation1 = "J. P. Puissant, R. V. D. Straeten, and T. Mens, “Resolving model inconsistencies using automated regression planning,” Software and System Modeling, vol. 14, no. 1, pp. 461–481, 2015."

      parser.parseAll(parser.citation, citation1) match {
         case parser.Success(matched: Citation,_) => {
            matched.title shouldBe "Resolving model inconsistencies using automated regression planning"
            matched.authors.size shouldBe 3
            matched.authors shouldEqual Seq("Puissant", "Straeten", "Mens")
         }

         case parser.Failure(msg,_) => fail("Parsing failed : " + msg)
         case parser.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }
}
