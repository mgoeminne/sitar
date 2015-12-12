package com.github.mgoeminne.sitar.test.apalike

import com.github.mgoeminne.sitar.parser.Citation
import com.github.mgoeminne.sitar.parser.apalike.parser
import org.scalatest.{FlatSpec, Matchers}


class ApalikeArticleTest extends FlatSpec with Matchers
{
   val p = parser.articleParser

   "Single author article citation" should "be correctly parsed" in {
      val citation = "Sidak, Z. (1967). Rectangular confidence regions for the means of multivariate normal distributions. Journal of the American Statistical Association, 62(318):pp. 626–633."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "Rectangular confidence regions for the means of multivariate normal distributions"
            matched.authors.size shouldBe 1
            matched.authors(0) shouldBe "Sidak"
         }

         case p.Failure(msg,_) => fail("Parsing failed : " + msg)
         case p.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }

   "Two authors article citation" should "be correctly parsed" in {
      val citation = "Kaplan, E. L. and Meier, P. (1958). Nonparametric estimation from incomplete observations. Journal of the American Statistical Association, 53(282):pp. 457–481."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "Nonparametric estimation from incomplete observations"
            matched.authors.size shouldBe 2
            matched.authors shouldEqual Seq("Kaplan", "Meier")
         }

         case p.Failure(msg,_) => fail("Parsing failed : " + msg)
         case p.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }

   "Three authors article citation" should "be correclty parsed" in {
      val citation1 = "Puissant, J. P., Straeten, R. V. D., and Mens, T. (2015). Resolving model inconsistencies using automated regression planning. Software and System Modeling, 14(1):461–481."

      p.parseAll(p.citation, citation1) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "Resolving model inconsistencies using automated regression planning"
            matched.authors.size shouldBe 3
            matched.authors shouldEqual Seq("Puissant", "Straeten", "Mens")
         }

         case p.Failure(msg,_) => fail("Parsing failed : " + msg)
         case p.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }
}
