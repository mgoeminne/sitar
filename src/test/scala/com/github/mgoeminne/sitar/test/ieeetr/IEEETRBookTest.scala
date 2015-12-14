package com.github.mgoeminne.sitar.parser.ieeetr

import com.github.mgoeminne.sitar.parser.{Book, Citation, ieeetr}
import org.scalatest.{FlatSpec, Matchers}

class IEEETRBookTest extends FlatSpec with Matchers
{
   val p = parser.bookParser


   "One author book citation" should "be correctly parsed" in {
      val citation = "H. Abut, ed., Vector Quantization. IEEE Press, 1990."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Book,_) => {
            matched.title shouldBe "Vector Quantization"
            matched.authors shouldEqual Seq("Abut")
            matched.year shouldEqual 1990
         }
      }
   }

   "Two authors book citation" should "be correctly parsed" in {
      val citation = "J. Heckman and E. Leamer, eds., Handbook of Econometrics. Elsevier, 2007."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "Handbook of Econometrics"
            matched.authors shouldEqual Seq("Heckman", "Leamer")
            matched.year shouldEqual 2007
         }
      }
   }

   "Two authors book citation" should "be correctly parsed, even if it has a volume" in {
      val citation = "J. Heckman and E. Leamer, eds., Handbook of Econometrics, vol. 6 of Handbook of Econometrics. Elsevier, 2007."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "Handbook of Econometrics"
            matched.authors shouldEqual Seq("Heckman", "Leamer")
            matched.year shouldEqual 2007
         }
      }
   }

   "Three authors book citation" should "be correctly parsed" in {
      val citation = "B. S. Bloom, J. T. Hastings, and G. F. Madaus, Handbook on Formative and Summative Evolution of Student Learning. New York: McGraw-Hill, 1971."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Book,_) => {
            matched.title shouldBe "Handbook on Formative and Summative Evolution of Student Learning"
            matched.authors shouldEqual Seq("Bloom", "Hastings", "Madaus")
            matched.year shouldBe 1971
         }
      }
   }
}
