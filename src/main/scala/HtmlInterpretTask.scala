package com.github.dmlap.shtml

import sbt._
import Keys._

object HtmlInterpretTask extends Plugin {
  val shtmlTemplateDir =
    SettingKey[File]("shtml-template-dir",
                     "The directory that contains the HTML templates that should be processed")
  val shtmlSettings: Seq[Project.Setting[_]] = 
    Seq(sourceGenerators in Compile <+= (shtmlTemplateDir in Compile, sourceManaged in Compile) map { (templateDir, targetDir) =>
      (templateDir ** "*.html").get map { file =>
        val name :: path = IO.pathSplit(templateDir.getName).toList
        val target = new File(targetDir, name + ".scala")
        IO.write(target, HtmlInterpreter.parse(path, name, IO.read(file)))
        target
      }
    })
}
