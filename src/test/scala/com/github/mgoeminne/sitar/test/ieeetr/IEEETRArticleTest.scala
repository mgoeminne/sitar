package com.github.mgoeminne.sitar.parser.ieeetr

import com.github.mgoeminne.sitar.parser.{Paper, Citation, ieeetr}
import org.scalatest.{FlatSpec, Matchers}


class IEEETRArticleTest extends FlatSpec with Matchers
{
   val p = parser.articleParser

   "Single author article citation" should "be correctly parsed" in {
      val citation = "Z. Sidak, “Rectangular confidence regions for the means of multivariate normal distributions,” Journal of the American Statistical Association, vol. 62, no. 318, pp. pp. 626–633, 1967."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Paper,_) => {
            matched.title shouldBe "Rectangular confidence regions for the means of multivariate normal distributions"
            matched.authors shouldBe Seq("Sidak")
            matched.year shouldBe 1967
            matched.in shouldBe "Journal of the American Statistical Association"
         }
      }
   }

   "Two authors article citation" should "be correctly parsed" in {
      val citation = "E. L. Kaplan and P. Meier, “Nonparametric estimation from incomplete observations,” Journal of the American Statistical Association, vol. 53, no. 282, pp. pp. 457–481, 1958."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Paper,_) => {
            matched.title shouldBe "Nonparametric estimation from incomplete observations"
            matched.authors shouldEqual Seq("Kaplan", "Meier")
            matched.in shouldBe "Journal of the American Statistical Association"
            matched.year shouldBe 1958
         }
      }
   }

   "Three authors article citation" should "be correclty parsed" in {
      val citation1 = "J. P. Puissant, R. V. D. Straeten, and T. Mens, “Resolving model inconsistencies using automated regression planning,” Software and System Modeling, vol. 14, no. 1, pp. 461–481, 2015."

      p.parseAll(p.citation, citation1) match {
         case p.Success(matched: Paper,_) => {
            matched.title shouldBe "Resolving model inconsistencies using automated regression planning"
            matched.authors shouldEqual Seq("Puissant", "Straeten", "Mens")
            matched.in shouldBe "Software and System Modeling"
            matched.year shouldBe 2015
         }
      }
   }
}
