[
  new File(basedir, "src/site/site.xml"),
  new File(basedir, "src/site/apt/index.apt.vm"),
  new File(basedir, "src/changes/changes.xml"),
  new File(basedir, "ebin/setup_std.app"),
  new File(basedir, "ebin/setup_std.appup")
].each { file ->
  if (!file.isFile()) {
    throw new IllegalStateException("The target file: " + file + " should have been created by the setup goal.");
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
  if (!dir.isDirectory()) {
    throw new IllegalStateException("The target directory: " + dir + " should have been created by the setup goal.");
  }
}

return true;
