package com.github.mgoeminne.sitar.parser.ieeetr

import com.github.mgoeminne.sitar.parser.{Paper, Citation, ieeetr}
import org.scalatest.{FlatSpec, Matchers}


class IEEETRInProceedingsTest extends FlatSpec with Matchers
{
   val p = parser.inProceedingsParser

   "Single author inproceedings citation" should "be correctly parsed" in {
      val citation = "G. Scanniello, “Source code survival with the Kaplan Meier estimator,” in Int’l Conf. Software Maintenance , 2011, pp. 524– 527."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Paper,_) => {
            matched.title shouldBe "Source code survival with the Kaplan Meier estimator"
            matched.authors shouldBe Seq("Scanniello")
            matched.in shouldBe "Int’l Conf. Software Maintenance"
            matched.year shouldBe 2011
         }
      }
   }

   "Two authors inproceedings citation" should "be correctly parsed" in {
      val citation = "P. Kyriakakis and A. Chatzigeorgiou, “Maintenance patterns of large-scale PHP web applications,” in Int’l Conf. Software Maintenance and Evolution , 2014, pp. 381–390."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Paper,_) => {
            matched.title shouldBe "Maintenance patterns of large-scale PHP web applications"
            matched.authors shouldEqual Seq("Kyriakakis", "Chatzigeorgiou")
            matched.in shouldBe "Int’l Conf. Software Maintenance and Evolution"
            matched.year shouldBe 2014
         }
      }
   }

   "Three authors inproceedings citation" should "be correclty parsed" in {
      val citation1 = "D. Qiu, B. Li, and  Z.  Su,  “An empirical analysis of the co-evolution of schema and code in database applications,” in Joint ESEC/FSE Conf. ACM , 2013."

      p.parseAll(p.citation, citation1) match {
         case p.Success(matched: Paper,_) => {
            matched.title shouldBe "An empirical analysis of the co-evolution of schema and code in database applications"
            matched.authors shouldEqual Seq("Qiu", "Li", "Su")
            matched.in shouldBe "Joint ESEC/FSE Conf. ACM"
            matched.year shouldBe 2013
         }
      }


      val citation2 = "M. Fanty, P. Schmid, and R. Cole, “City name recognition over the telephone,” in Proc. International Conference on Acoustics, Speech and Signal Processing, vol. I, (Minneapolis, U.S.A.), pp. 549-552, April 1993. "

      p.parseAll(p.citation, citation2) match {
         case p.Success(matched: Paper,_) => {
            matched.title shouldBe "City name recognition over the telephone"
            matched.authors shouldEqual Seq("Fanty", "Schmid", "Cole")
            matched.in shouldBe "Proc. International Conference on Acoustics, Speech and Signal Processing"
            matched.year shouldBe 1993
         }
      }


      val citation3 = "Y. Linde, A. Buzo, and R.M. Gray, “An algorithm for vector quantizer design,” IEEE Transactions on Communications, vol. 28, pp. 84-95, January 1980. "

      p.parseAll(p.citation, citation3) match {
         case p.Success(matched: Paper,_) => {
            matched.title shouldBe "An algorithm for vector quantizer design"
            matched.authors shouldEqual Seq("Linde", "Buzo", "Gray")
            matched.in shouldBe "IEEE Transactions on Communications"
            matched.year shouldBe 1980
         }
      }
   }
}
