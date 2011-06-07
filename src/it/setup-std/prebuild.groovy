[
  new File(basedir, "src/site/site.xml"),
  new File(basedir, "src/site/apt/index.apt.vm"),
  new File(basedir, "src/changes/changes.xml"),
  new File(basedir, "ebin/setup-std.app"),
  new File(basedir, "ebin/setup-std.appup")
].each { file ->
  if (file.isFile()) {
    throw new IllegalStateException("The target file: " + file + " must not exist before test is run.");
  }
}

[
  new File(basedir, "ebin"),
  new File(basedir, "src"),
  new File(basedir, "include"),
  new File(basedir, "priv"),
  new File(basedir, "test"),
  new File(basedir, "test_include"),
  new File(basedir, "test_priv")
].each { dir ->
  if (dir.isDirectory()) {
    throw new IllegalStateException("The target directory: " + dir + " must not exist before test is run.");
  }
}
    
return true;
