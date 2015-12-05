package com.github.mgoeminne.sitar.parser.abbrv

import com.github.mgoeminne.sitar.parser.{Citation, abbrv}
import org.scalatest.{FlatSpec, Matchers}


class ABBRVInProceedingsTest extends FlatSpec with Matchers
{
   val p = parser.inProceedingsParser

   "Single author inproceedings citation" should "be correctly parsed" in {
      val citation = "G. Scanniello. Source code survival with the Kaplan Meier estimator. In Software Maintenance (ICSM), 2011 27th IEEE International Conference on, pages 524–527, Sept 2011."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.authors shouldBe Seq("Scanniello")
            matched.title shouldBe "Source code survival with the Kaplan Meier estimator"
         }

      }
   }

   "Two authors inproceedings citation" should "be correctly parsed" in {
      val citation = "P. Kyriakakis and A. Chatzigeorgiou. Maintenance patterns of large-scale PHP web applications. In Software Maintenance and Evolution (ICSME), 2014 IEEE International Conference on, pages 381–390, Sept 2014."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) =>
         {
            matched.authors shouldEqual Seq("Kyriakakis", "Chatzigeorgiou")
            matched.title shouldBe "Maintenance patterns of large-scale PHP web applications"
         }
      }
   }

   "Three authors inproceedings citation" should "be correclty parsed" in {
      val citation1 = "D. Qiu, B. Li, and Z. Su. An empirical analysis of the co-evolution of schema and code in database applications. In B. Meyer, L. Baresi, and M. Mezini, editors, ESEC/SIGSOFT FSE, pages 125–135. ACM, 2013."

      p.parseAll(p.citation, citation1) match {
         case p.Success(matched: Citation,_) => {
            matched.authors shouldEqual Seq("Qiu", "Li", "Su")
            matched.title shouldBe "An empirical analysis of the co-evolution of schema and code in database applications"
         }
      }
   }
}
