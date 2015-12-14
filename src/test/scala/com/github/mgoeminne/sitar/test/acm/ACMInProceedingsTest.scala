package com.github.mgoeminne.sitar.parser.acm

import com.github.mgoeminne.sitar.parser.{Paper, Citation}
import org.scalatest.{FlatSpec, Matchers}


class ACMInProceedingsTest extends FlatSpec with Matchers
{
   val p = parser.inProceedingsParser

   "Single author inproceedings citation" should "be correctly parsed" in {
      val citation = "Scanniello, G. Source code survival with the Kaplan Meier estimator. In Software Maintenance (ICSM), 2011 27th IEEE International Conference on (Sept 2011), pp. 524–527."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Paper,_) => {
            matched.title shouldBe "Source code survival with the Kaplan Meier estimator"
            matched.authors shouldEqual Seq("Scanniello")
            matched.in shouldBe "Software Maintenance (ICSM)"
            matched.year shouldBe 2011
         }
      }
   }

   "Two authors inproceedings citation" should "be correctly parsed" in {
      val citation = "Kyriakakis, P., and Chatzigeorgiou, A. Maintenance patterns of large-scale PHP web applications. In Software Maintenance and Evolution (ICSME), 2014 IEEE International Conference on (Sept 2014), pp. 381–390."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Paper,_) => {
            matched.title shouldBe "Maintenance patterns of large-scale PHP web applications"
            matched.authors shouldEqual Seq("Kyriakakis", "Chatzigeorgiou")
            matched.in shouldBe "Software Maintenance and Evolution (ICSME)"
            matched.year shouldBe 2014
         }
      }
   }

   "Three authors inproceedings citation" should "be correclty parsed" in {
      val citation1 = "Qiu, D., Li, B., and Su, Z. An empirical analysis of the co-evolution of schema and code in database applications. In ESEC/SIGSOFT FSE (2013), B. Meyer, L. Baresi, and M. Mezini, Eds., ACM, pp. 125– 135."

      p.parseAll(p.citation, citation1) match {
         case p.Success(matched: Paper,_) => {
            matched.title shouldBe "An empirical analysis of the co-evolution of schema and code in database applications"
            matched.authors shouldEqual Seq("Qiu", "Li", "Su")
            matched.in shouldBe "ESEC/SIGSOFT FSE (2013)"
            matched.year shouldBe 2013
         }
      }
   }
}
