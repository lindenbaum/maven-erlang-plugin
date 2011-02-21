[
  new File(basedir, "src/site/site.xml"),
  new File(basedir, "src/site/apt/index.apt.vm"),
  new File(basedir, "src/changes/changes.xml"),
  new File(basedir, "setup-rel.rel"),
  new File(basedir, "sys.config")
].each { file ->
  if (!file.isFile()) {
    throw new IllegalStateException("The target file: " + file + " should have been created by the setup goal.");
  }
}

return true;
