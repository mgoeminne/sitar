package com.github.mgoeminne.sitar.parser.abbrv

import com.github.mgoeminne.sitar.parser.{Citation}
import org.scalatest.{FlatSpec, Matchers}


class ABBRVArticleTest extends FlatSpec with Matchers
{
   val p = parser.articleParser

   "Single author article citation" should "be correctly parsed" in {
      val citation = "Z. Sidak. Rectangular confidence regions for the means of multivariate normal distributions. Journal of the American Statistical Association, 62(318):pp. 626–633, 1967."
      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.authors shouldBe Seq("Sidak")
            matched.title shouldBe "Rectangular confidence regions for the means of multivariate normal distributions"
         }
      }
   }

   it should "be correctly parsed, even if the author's lastname is composite" in {
      val citation = "Z. van der Sidak. Rectangular confidence regions for the means of multivariate normal distributions. Journal of the American Statistical Association, 62(318):pp. 626–633, 1967."
      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.authors shouldBe Seq("Sidak")
            matched.title shouldBe "Rectangular confidence regions for the means of multivariate normal distributions"
         }
      }
   }

   "Two authors article citation" should "be correctly parsed" in {
      val citation = "E. L. Kaplan and P. Meier. Nonparametric estimation from incomplete observations. Journal of the American Statistical Association, 53(282):pp. 457–481, 1958."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.authors shouldEqual Seq("Kaplan", "Meier")
            matched.title shouldBe "Nonparametric estimation from incomplete observations"
         }
      }
   }

   "Three authors article citation" should "be correclty parsed" in {
      val citation1 = "J. P. Puissant, R. V. D. Straeten, and T. Mens. Resolving model inconsistencies using automated regression planning. Software and System Modeling, 14(1):461–481, 2015."

      p.parseAll(p.citation, citation1) match {
         case p.Success(matched: Citation,_) => {
            matched.authors shouldEqual Seq("Puissant", "Straeten", "Mens")
            matched.title shouldBe "Resolving model inconsistencies using automated regression planning"
         }
      }
   }
}
