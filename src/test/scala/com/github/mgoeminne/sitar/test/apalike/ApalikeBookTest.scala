package com.github.mgoeminne.sitar.test.apalike

import com.github.mgoeminne.sitar.parser.Citation
import com.github.mgoeminne.sitar.parser.apalike.parser
import org.scalatest.{FlatSpec, Matchers}

class ApalikeBookTest extends FlatSpec with Matchers
{
   val p = parser.bookParser


   "One author book citation" should "be correctly parsed" in {
      val citation = "Martin, R. C. (2009). Clean Code: A handbook of agile software craftsmanship. Prentice Hall."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "Clean Code: A handbook of agile software craftsmanship"
            matched.authors.size shouldBe 1
            matched.authors(0) shouldEqual "Martin"
         }
      }
   }

   "Two authors book citation" should "be correctly parsed" in {
      val citation = "Heckman, J. and Leamer, E., editors (2007). Handbook of Econometrics. Elsevier."

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

   it should "be correctly parsed, even if it has a volume" in {
      val citation = "Heckman, J. and Leamer, E., editors (2007). Handbook of Econometrics, volume 6 of Handbook of Econometrics. Elsevier."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            matched.title shouldBe "Handbook of Econometrics"
            matched.authors.size shouldBe 2
            matched.authors shouldEqual Seq("Heckman", "Leamer")
         }
      }
   }

   "Three authors book citation" should "be correctly parsed" in {
      val citation = "Bloom, B. S., Hastings, J. T., and Madaus, G. F. (1971). Handbook on Formative and Summative Evolution of Student Learning. McGraw-Hill, New York."

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
