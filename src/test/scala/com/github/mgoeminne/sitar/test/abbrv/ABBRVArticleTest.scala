package com.github.mgoeminne.sitar.test.abbrv

import com.github.mgoeminne.sitar.parser.{Citation, acm}
import org.scalatest.{FlatSpec, Matchers}


class ABBRVArticleTest extends FlatSpec with Matchers
{
   val parser = acm.articleParser

   "Single author article citation" should "be correctly parsed" in {
      val citation = "Sidak, Z. Rectangular confidence regions for the means of multivariate normal distributions. Journal of the American Statistical Association 62, 318 (1967), pp. 626–633."

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
      val citation = "Kaplan, E. L., and Meier, P. Nonparametric estimation from incomplete observations. Journal of the American Statistical Association 53, 282 (1958), pp. 457–481."

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
      val citation1 = "Puissant, J. P., Straeten, R. V. D., and Mens, T. Resolving model inconsistencies using automated regression planning. Software and System Modeling 14, 1 (2015), 461–481."

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
