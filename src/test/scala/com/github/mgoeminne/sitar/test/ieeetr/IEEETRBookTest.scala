package com.github.mgoeminne.sitar.parser.ieeetr

import com.github.mgoeminne.sitar.parser.{Citation, ieeetr}
import org.scalatest.{FlatSpec, Matchers}

class IEEETRBookTest extends FlatSpec with Matchers
{
   val p = parser.bookParser


   "One author book citation" should "be correctly parsed" in {
      val citation = "H. Abut, ed., Vector Quantization. IEEE Press, 1990."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "Vector Quantization"
            matched.authors.size shouldBe 1
            matched.authors(0) shouldEqual "Abut"
         }
      }
   }

   "Two authors book citation" should "be correctly parsed" in {
      val citation = "J. Heckman and E. Leamer, eds., Handbook of Econometrics. Elsevier, 2007."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "Handbook of Econometrics"
            matched.authors.size shouldBe 2
            matched.authors shouldEqual Seq("Heckman", "Leamer")
         }

         case p.Failure(msg,_) => fail("Parsing failed : " + msg)
         case p.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }

   "Two authors book citation" should "be correctly parsed, even if it has a volume" in {
      val citation = "J. Heckman and E. Leamer, eds., Handbook of Econometrics, vol. 6 of Handbook of Econometrics. Elsevier, 2007."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "Handbook of Econometrics"
            matched.authors.size shouldBe 2
            matched.authors shouldEqual Seq("Heckman", "Leamer")
         }

         case p.Failure(msg,_) => fail("Parsing failed : " + msg)
         case p.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }

   "Three authors book citation" should "be correctly parsed" in {
      val citation = "B. S. Bloom, J. T. Hastings, and G. F. Madaus, Handbook on Formative and Summative Evolution of Student Learning. New York: McGraw-Hill, 1971."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "Handbook on Formative and Summative Evolution of Student Learning"
            matched.authors.size shouldBe 3
            matched.authors shouldEqual Seq("Bloom", "Hastings", "Madaus")
         }

         case p.Failure(msg,_) => fail("Parsing failed : " + msg)
         case p.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }
}
