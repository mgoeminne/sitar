package com.github.mgoeminne.sitar.test.ieeetr

import com.github.mgoeminne.sitar.parser.{Citation, ieeetr}
import org.scalatest.{FlatSpec, Matchers}

class IEEETRBookTest extends FlatSpec with Matchers
{
   val parser = ieeetr.bookParser


   "One author book citation" should "be correctly parsed" in {
      val citation = "H. Abut, ed., Vector Quantization. IEEE Press, 1990."

      parser.parseAll(parser.citation, citation) match {
         case parser.Success(matched: Citation,_) => {
            matched.title shouldBe "Vector Quantization"
            matched.authors.size shouldBe 1
            matched.authors(0) shouldEqual "Abut"
         }

         case parser.Failure(msg,_) => fail("Parsing failed : " + msg)
         case parser.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }

   "Two authors book citation" should "be correctly parsed" in {
      val citation = "J. Heckman and E. Leamer, eds., Handbook of Econometrics, vol. 6 of Handbook of Econometrics. Elsevier, 2007."

      parser.parseAll(parser.citation, citation) match {
         case parser.Success(matched: Citation,_) => {
            matched.title shouldBe "Handbook of Econometrics"
            matched.authors.size shouldBe 2
            matched.authors shouldEqual Seq("Heckman", "Leamer")
         }

         case parser.Failure(msg,_) => fail("Parsing failed : " + msg)
         case parser.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }

   "Three authors book citation" should "be correctly parsed" in {
      val citation = "B. S. Bloom, J. T. Hastings, and G. F. Madaus, Handbook on Formative and Summative Evolution of Student Learning. New York: McGraw-Hill, 1971."

      parser.parseAll(parser.citation, citation) match {
         case parser.Success(matched: Citation,_) => {
            matched.title shouldBe "Handbook on Formative and Summative Evolution of Student Learning"
            matched.authors.size shouldBe 3
            matched.authors shouldEqual Seq("Bloom", "Hastings", "Madaus")
         }

         case parser.Failure(msg,_) => fail("Parsing failed : " + msg)
         case parser.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }
}
