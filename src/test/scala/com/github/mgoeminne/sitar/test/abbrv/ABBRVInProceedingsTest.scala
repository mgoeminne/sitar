package com.github.mgoeminne.sitar.test.abbrv

import com.github.mgoeminne.sitar.parser.{Citation, abbrv}
import org.scalatest.{FlatSpec, Matchers}


class ABBRVInProceedingsTest extends FlatSpec with Matchers
{
   val parser = abbrv.inProceedingsParser

   "Single author inproceedings citation" should "be correctly parsed" in {
      val citation = "Scanniello, G. Source code survival with the Kaplan Meier estimator. In Software Maintenance (ICSM), 2011 27th IEEE International Conference on (Sept 2011), pp. 524–527."

      parser.parseAll(parser.citation, citation) match {
         case parser.Success(matched: Citation,_) => {
            matched.title shouldBe "Source code survival with the Kaplan Meier estimator"
            matched.authors.size shouldBe 1
            matched.authors(0) shouldBe "Scanniello"
         }

      }
   }

   "Two authors inproceedings citation" should "be correctly parsed" in {
      val citation = "Kyriakakis, P., and Chatzigeorgiou, A. Maintenance patterns of large-scale PHP web applications. In Software Maintenance and Evolution (ICSME), 2014 IEEE International Conference on (Sept 2014), pp. 381–390."

      parser.parseAll(parser.citation, citation) match {
         case parser.Success(matched: Citation,_) => {
            matched.title shouldBe "Maintenance patterns of large-scale PHP web applications"
            matched.authors.size shouldBe 2
            matched.authors shouldEqual Seq("Kyriakakis", "Chatzigeorgiou")
         }

         case parser.Failure(msg,_) => fail("Parsing failed : " + msg)
         case parser.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }

   "Three authors inproceedings citation" should "be correclty parsed" in {
      val citation1 = "Qiu, D., Li, B., and Su, Z. An empirical analysis of the co-evolution of schema and code in database applications. In ESEC/SIGSOFT FSE (2013), B. Meyer, L. Baresi, and M. Mezini, Eds., ACM, pp. 125– 135."

      parser.parseAll(parser.citation, citation1) match {
         case parser.Success(matched: Citation,_) => {
            matched.title shouldBe "An empirical analysis of the co-evolution of schema and code in database applications"
            matched.authors.size shouldBe 3
            matched.authors shouldEqual Seq("Qiu", "Li", "Su")
         }

         case parser.Failure(msg,_) => fail("Parsing failed : " + msg)
         case parser.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }
}
