package com.github.mgoeminne.sitar

import java.text.Normalizer

/**
  * A representation of a scientific paper
  */
case class Paper(title: String, authors: Seq[String])
{
   override def toString() = """"""" + title + """" by """ + authors.mkString(", ")
}

object Paper
{
   def normalizeTitle(title: String) =
   {
      Normalizer.normalize(title, Normalizer.Form.NFD)
         .replaceAll("[\\p{InCombiningDiacriticalMarks}]", "")
         .toLowerCase
         .replaceAll("""[^\w]""", "")
   }

   def similar(p1: Paper, p2: Paper): Boolean =
   {
      (normalizeTitle(p1.title) == normalizeTitle(p2.title)) &&
      (p1.authors zip p2.authors).forall(a => a._1 == a._2)
   }
}