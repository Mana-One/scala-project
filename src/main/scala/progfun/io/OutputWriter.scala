package fr.esgi.al.progfun.io

import better.files.File


object OutputWriter {
  def write(appConfig: AppConfig, output: String): Unit = {
    File(appConfig.outputFile)
      .createFileIfNotExists(true)
      .overwrite(output)
    ()
  }
}