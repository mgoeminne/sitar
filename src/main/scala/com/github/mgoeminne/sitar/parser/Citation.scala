package com.github.mgoeminne.sitar.parser

import java.text.Normalizer

/**
  * A representation of a scientific paper
  */
abstract case class Citation(title: String, authors: Seq[String], year: Int)
{
   override def toString() = """"""" + title + """" by """ + authors.mkString(", ")
}

class Paper(title: String, authors: Seq[String], year: Int, val in: String) extends Citation(title, authors, year)
class Book(title: String, authors: Seq[String], year: Int) extends Citation(title, authors, year)


object Citation
{
   def normalizeTitle(title: String) =
   {
      Normalizer.normalize(title, Normalizer.Form.NFD)
         .replaceAll("[\\p{InCombiningDiacriticalMarks}]", "")
         .toLowerCase
         .replaceAll("""[^\w]""", "")
   }

   def similar(c1: Citation, c2: Citation): Boolean =
   {
      (normalizeTitle(c1.title) == normalizeTitle(c2.title)) &&
         (c1.authors zip c2.authors).forall(a => a._1 == a._2)
   }
}