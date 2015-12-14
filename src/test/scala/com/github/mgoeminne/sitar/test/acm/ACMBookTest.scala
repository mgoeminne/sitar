package com.github.mgoeminne.sitar.parser.acm

import com.github.mgoeminne.sitar.parser.{Book, Citation}
import org.scalatest.{FlatSpec, Matchers}

class ACMBookTest extends FlatSpec with Matchers
{
   val p = parser.bookParser


   "One author book citation" should "be correctly parsed" in {
      val citation = "Martin, R. C. Clean Code: A handbook of agile software craftsmanship. Prentice Hall, 2009."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Book,_) => {
            matched.title shouldBe "Clean Code: A handbook of agile software craftsmanship"
            matched.authors shouldEqual Seq("Martin")
            matched.year shouldBe 2009
         }
      }
   }

   "Two authors book citation" should "be correctly parsed" in {
      val citation = "Heckman, J., and Leamer, E., Eds. Handbook of Econometrics. Elsevier, 2007."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Book,_) => {
            matched.title shouldBe "Handbook of Econometrics"
            matched.authors shouldEqual Seq("Heckman", "Leamer")
            matched.year shouldBe 2007
         }
      }
   }

   it should "be correctly parser, even in the volume is present" in {
      val citation = "Heckman, J., and Leamer, E., Eds. Handbook of Econometrics, vol. 6 of Handbook of Econometrics. Elsevier, 2007."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Book,_) => {
            matched.title shouldBe "Handbook of Econometrics"
            matched.authors shouldEqual Seq("Heckman", "Leamer")
            matched.year shouldBe 2007
         }
      }
   }

   "Three authors book citation" should "be correctly parsed" in {
      val citation = "Bloom, B. S., Hastings, J. T., and Madaus, G. F. Handbook on Formative and Summative Evolution of Student Learning. McGraw-Hill, New York, 1971."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "Handbook on Formative and Summative Evolution of Student Learning"
            matched.authors shouldEqual Seq("Bloom", "Hastings", "Madaus")
            matched.year shouldBe 1971
         }
      }
   }
}
