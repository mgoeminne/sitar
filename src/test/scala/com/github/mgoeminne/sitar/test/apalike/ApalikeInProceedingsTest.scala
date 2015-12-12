package com.github.mgoeminne.sitar.test.apalike

import com.github.mgoeminne.sitar.parser.Citation
import com.github.mgoeminne.sitar.parser.apalike.parser
import org.scalatest.{FlatSpec, Matchers}


class ApalikeInProceedingsTest extends FlatSpec with Matchers
{
   val p = parser.inProceedingsParser

   "Single author inproceedings citation" should "be correctly parsed" in {
      val citation = "Scanniello, G. (2011). Source code survival with the Kaplan Meier estimator. In Software Maintenance (ICSM), 2011 27th IEEE International Conference on, pages 524–527."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "Source code survival with the Kaplan Meier estimator"
            matched.authors.size shouldBe 1
            matched.authors(0) shouldBe "Scanniello"
         }

         case p.Failure(msg,_) => fail("Parsing failed : " + msg)
         case p.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }

   "Two authors inproceedings citation" should "be correctly parsed" in {
      val citation = "Kyriakakis, P. and Chatzigeorgiou, A. (2014). Maintenance patterns of large-scale PHP web applications. In Software Maintenance and Evolution (ICSME), 2014 IEEE Inter- national Conference on, pages 381–390."

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
      val citation1 = "Qiu, D., Li, B., and Su, Z. (2013). An empirical analysis of the co-evolution of schema and code in database applications. In Meyer, B., Baresi, L., and Mezini, M., editors, ESEC/SIGSOFT FSE, pages 125–135. ACM."

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
