package com.github.mgoeminne.sitar.test.ieeetr

import com.github.mgoeminne.sitar.parser.{Citation, ieeetr}
import com.github.mgoeminne.sitar.test.ieeetr
import org.scalatest.{FlatSpec, Matchers}


class IEEETRInProceedingsTest extends FlatSpec with Matchers
{
   val parser = ieeetr.inProceedingsParser

   "Single author inproceedings citation" should "be correctly parsed" in {
      val citation = "G.  Scanniello,  “Source  code  survival  with  the  Kaplan  Meier estimator,” in Int’l Conf. Software Maintenance , 2011, pp. 524– 527."

      parser.parseAll(parser.citation, citation) match {
         case parser.Success(matched: Citation,_) => {
            matched.title shouldBe "Source code survival with the Kaplan Meier estimator"
            matched.authors.size shouldBe 1
            matched.authors(0) shouldBe "Scanniello"
         }

         case parser.Failure(msg,_) => fail("Parsing failed : " + msg)
         case parser.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }

   "Two authors inproceedings citation" should "be correctly parsed" in {
      val citation = "P.  Kyriakakis  and  A.  Chatzigeorgiou,  “Maintenance  patterns of  large-scale  PHP  web  applications,”  in Int’l  Conf.  Software Maintenance and Evolution , 2014, pp. 381–390."

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
      val citation1 = "D.  Qiu,  B.  Li, and  Z.  Su,  “An  empirical  analysis  of  the  co-evolution of schema and code in database applications,” in Joint ESEC/FSE Conf. ACM , 2013."

      parser.parseAll(parser.citation, citation1) match {
         case parser.Success(matched: Citation,_) => {
            matched.title shouldBe "An empirical analysis of the co-evolution of schema and code in database applications"
            matched.authors.size shouldBe 3
            matched.authors shouldEqual Seq("Qiu", "Li", "Su")
         }

         case parser.Failure(msg,_) => fail("Parsing failed : " + msg)
         case parser.Error(msg,_) => fail("Parsing error : " + msg)
      }


      val citation2 = "M. Fanty, P. Schmid, and R. Cole, “City name recognition over the telephone,” in Proc. International Conference on Acoustics, Speech and Signal Processing, vol. I, (Minneapolis, U.S.A.), pp. 549-552, April 1993. "

      parser.parseAll(parser.citation, citation2) match {
         case parser.Success(matched: Citation,_) => {
            matched.title shouldBe "City name recognition over the telephone"
            matched.authors.size shouldBe 3
            matched.authors shouldEqual Seq("Fanty", "Schmid", "Cole")
         }

         case parser.Failure(msg,_) => fail("Parsing failed : " + msg)
         case parser.Error(msg,_) => fail("Parsing error : " + msg)
      }


      val citation3 = "Y. Linde, A. Buzo, and R.M. Gray, “An algorithm for vector quantizer design,” IEEE Transactions on Communications, vol. 28, pp. 84-95, January 1980. "

      parser.parseAll(parser.citation, citation3) match {
         case parser.Success(matched: Citation,_) => {
            matched.title shouldBe "An algorithm for vector quantizer design"
            matched.authors.size shouldBe 3
            matched.authors shouldEqual Seq("Linde", "Buzo", "Gray")
         }

         case parser.Failure(msg,_) => fail("Parsing failed : " + msg)
         case parser.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }
}
