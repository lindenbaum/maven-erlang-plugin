[
  new File(basedir, "src/site/site.xml"),
  new File(basedir, "src/site/apt/index.apt.vm"),
  new File(basedir, "src/changes/changes.xml"),
  new File(basedir, "setup_rel.rel"),
  new File(basedir, "relup"),
  new File(basedir, "sys.config")
].each { file ->
  if (file.isFile()) {
    throw new IllegalStateException("The target file: " + file + " must not exist before test is run.");
  }
}

[
  new File(basedir, "src"),
].each { dir ->
  if (dir.isDirectory()) {
    throw new IllegalStateException("The target directory: " + dir + " must not exist before test is run.");
  }
}
    
return true;
