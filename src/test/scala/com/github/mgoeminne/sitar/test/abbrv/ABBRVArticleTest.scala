package com.github.mgoeminne.sitar.test.abbrv

import com.github.mgoeminne.sitar.parser.{abbrv, Citation}
import org.scalatest.{FlatSpec, Matchers}


class ABBRVArticleTest extends FlatSpec with Matchers
{
   val parser = abbrv.articleParser

   "Single author article citation" should "be correctly parsed" in {
      val citation = "Z. Sidak. Rectangular confidence regions for the means of multivariate normal distributions. Journal of the American Statistical Association, 62(318):pp. 626–633, 1967."
      parser.parseAll(parser.citation, citation) match {
         case parser.Success(matched: Citation,_) => {
            matched.title shouldBe "Rectangular confidence regions for the means of multivariate normal distributions"
            matched.authors.size shouldBe 1
            matched.authors(0) shouldBe "Sidak"
         }
      }
   }

   it should "be correctly parsed, even if the author's lastname is composite" in {
      val citation = "Z. van der Sidak. Rectangular confidence regions for the means of multivariate normal distributions. Journal of the American Statistical Association, 62(318):pp. 626–633, 1967."
      parser.parseAll(parser.citation, citation) match {
         case parser.Success(matched: Citation,_) => {
            matched.title shouldBe "Rectangular confidence regions for the means of multivariate normal distributions"
            matched.authors.size shouldBe 1
            matched.authors(0) shouldBe "Sidak"
         }
      }
   }

   "Two authors article citation" should "be correctly parsed" in {
      val citation = "E. L. Kaplan and P. Meier. Nonparametric estimation from incomplete observations. Journal of the American Statistical Association, 53(282):pp. 457–481, 1958."

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
      val citation1 = "J. P. Puissant, R. V. D. Straeten, and T. Mens. Resolving model inconsistencies using automated regression planning. Software and System Modeling, 14(1):461–481, 2015."

      parser.parseAll(parser.citation, citation1) match {
         case parser.Success(matched: Citation,_) => {
            matched.title shouldBe "Resolving model inconsistencies using automated regression planning"
            matched.authors.size shouldBe 3
            matched.authors shouldEqual Seq("Puissant", "Straeten", "Mens")
         }
      }
   }
}
