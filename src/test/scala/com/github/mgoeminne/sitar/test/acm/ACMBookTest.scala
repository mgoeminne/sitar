package com.github.mgoeminne.sitar.parser.acm

import com.github.mgoeminne.sitar.parser.{Citation}
import org.scalatest.{FlatSpec, Matchers}

class ACMBookTest extends FlatSpec with Matchers
{
   val p = parser.bookParser


   "One author book citation" should "be correctly parsed" in {
      val citation = "Ross, S. M. On the time to first failure in multicomponent exponential reliability systems. Stochastic Processes and their Applications 4, 2 (1976), 167 – 173."

      p.parseAll(p.citation, citation) match {
         case p.Success(matched: Citation,_) => {
            println(matched)
            matched.title shouldBe "On the time to first failure in multicomponent exponential reliability systems"
            matched.authors.size shouldBe 1
            matched.authors(0) shouldEqual "Ross"
         }

         case p.Failure(msg,_) => fail("Parsing failed : " + msg)
         case p.Error(msg,_) => fail("Parsing error : " + msg)
      }
   }

   "Two authors book citation" should "be correctly parsed" in {
      val citation = "Heckman, J., and Leamer, E., Eds. Handbook of Econometrics. Elsevier, 2007."

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

   it should "be correctly parser, even in the volume is present" in {
      val citation = "Heckman, J., and Leamer, E., Eds. Handbook of Econometrics, vol. 6 of Handbook of Econometrics. Elsevier, 2007."

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
      val citation = "Bloom, B. S., Hastings, J. T., and Madaus, G. F. Handbook on Formative and Summative Evolution of Student Learning. McGraw-Hill, New York, 1971."

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
