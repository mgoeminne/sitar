package com.github.mgoeminne.sitar.parser.acm

import com.github.mgoeminne.sitar.parser.{Paper, Citation}
import org.scalatest.{FlatSpec, Matchers}


class ACMArticleTest extends FlatSpec with Matchers
{
   val p = parser.articleParser

   "Single author article citation" should "be correctly parsed" in {
      val citation = "Sidak, Z. Rectangular confidence regions for the means of multivariate normal distributions. Journal of the American Statistical Association 62, 318 (1967), pp. 626–633."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Paper,_) => {
            matched.title shouldBe "Rectangular confidence regions for the means of multivariate normal distributions"
            matched.authors shouldBe Seq("Sidak")
            matched.in shouldBe "Journal of the American Statistical Association"
            matched.year shouldBe 1967
         }

         case p.Failure(msg,_) => fail("Parsing failed : " + msg)
         case p.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }

   "Two authors article citation" should "be correctly parsed" in {
      val citation = "Kaplan, E. L., and Meier, P. Nonparametric estimation from incomplete observations. Journal of the American Statistical Association 53, 282 (1958), pp. 457–481."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Paper,_) => {
            matched.title shouldBe "Nonparametric estimation from incomplete observations"
            matched.authors shouldEqual Seq("Kaplan", "Meier")
            matched.in shouldEqual "Journal of the American Statistical Association"
            matched.year shouldEqual 1958
         }
      }
   }

   "Three authors article citation" should "be correclty parsed" in {
      val citation1 = "Puissant, J. P., Straeten, R. V. D., and Mens, T. Resolving model inconsistencies using automated regression planning. Software and System Modeling 14, 1 (2015), 461–481."

      p.parseAll(p.citation, citation1) match {
         case p.Success(matched: Paper,_) => {
            matched.title shouldBe "Resolving model inconsistencies using automated regression planning"
            matched.authors shouldEqual Seq("Puissant", "Straeten", "Mens")
            matched.in shouldEqual "Software and System Modeling"
            matched.year shouldEqual 2015
         }
      }
   }
}
