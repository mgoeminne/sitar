package com.github.mgoeminne.sitar.parser

/**
  * A citation
  * @param authors A sequence of authors' last names
  * @param title the title associated to the citation
  */
case class Citation(authors: Seq[String], title: String)
