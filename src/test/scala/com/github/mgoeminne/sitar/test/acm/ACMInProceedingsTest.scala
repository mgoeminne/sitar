package com.github.mgoeminne.sitar.parser.acm

import com.github.mgoeminne.sitar.parser.{Citation}
import org.scalatest.{FlatSpec, Matchers}


class ACMInProceedingsTest extends FlatSpec with Matchers
{
   val p = parser.inProceedingsParser

   "Single author inproceedings citation" should "be correctly parsed" in {
      val citation = "Scanniello, G. Source code survival with the Kaplan Meier estimator. In Software Maintenance (ICSM), 2011 27th IEEE International Conference on (Sept 2011), pp. 524–527."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "Source code survival with the Kaplan Meier estimator"
            matched.authors.size shouldBe 1
            matched.authors(0) shouldBe "Scanniello"
         }

      }
   }

   "Two authors inproceedings citation" should "be correctly parsed" in {
      val citation = "Kyriakakis, P., and Chatzigeorgiou, A. Maintenance patterns of large-scale PHP web applications. In Software Maintenance and Evolution (ICSME), 2014 IEEE International Conference on (Sept 2014), pp. 381–390."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "Maintenance patterns of large-scale PHP web applications"
            matched.authors.size shouldBe 2
            matched.authors shouldEqual Seq("Kyriakakis", "Chatzigeorgiou")
         }

         case p.Failure(msg,_) => fail("Parsing failed : " + msg)
         case p.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }

   "Three authors inproceedings citation" should "be correclty parsed" in {
      val citation1 = "Qiu, D., Li, B., and Su, Z. An empirical analysis of the co-evolution of schema and code in database applications. In ESEC/SIGSOFT FSE (2013), B. Meyer, L. Baresi, and M. Mezini, Eds., ACM, pp. 125– 135."

      p.parseAll(p.citation, citation1) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "An empirical analysis of the co-evolution of schema and code in database applications"
            matched.authors.size shouldBe 3
            matched.authors shouldEqual Seq("Qiu", "Li", "Su")
         }

         case p.Failure(msg,_) => fail("Parsing failed : " + msg)
         case p.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }
}
