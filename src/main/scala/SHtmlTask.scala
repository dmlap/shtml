package com.github.dmlap.shtml

import sbt._
import Keys._
import Project.Initialize

object SHtmlTask extends Plugin {
  val shtmlTemplateDir =
    SettingKey[File]("shtml-template-dir",
                     "The directory that contains the HTML templates that should be processed")
  def shtmlGeneratorTask: Initialize[Task[Seq[File]]] = 
    (shtmlTemplateDir in Compile,
     sourceManaged in Compile,
     organization,
     streams) map { (templateDir, targetDir, organization, streams) =>
      val templates = (templateDir ** "*.html").get
      if (templates.size == 0) {
        streams.log.warn("No templates found in " + templateDir)
      } else {
        streams.log.info("Compiling " + templates.size + " templates in " + templateDir + " to " + targetDir)
      }
      templates map { file =>
        val filename = file.getName
        val firstDotIx = filename.indexOf(".")
        val name = if (firstDotIx >= 0) {
          filename.substring(0, firstDotIx)
        } else {
          filename
        }
        val path =
          organization.split("\\.").toList ::: IO.pathSplit(IO.relativize(templateDir, file) getOrElse file.getPath).toList.tail
        val target = new File(targetDir, name + ".scala")
        IO.write(target, SHtml.parse(path, name, IO.read(file)))
        target
      }
    }
  val shtmlSettings: Seq[Project.Setting[_]] = 
    Seq(shtmlTemplateDir in Compile <<= (sourceDirectory in Runtime)(_ / "webapp"),
        sourceGenerators in Compile <+= shtmlGeneratorTask)
}
