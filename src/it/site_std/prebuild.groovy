[
  new File(basedir, "target/profiling-reports"),
  new File(basedir, "target/coverage-reports")
].each { file ->
  if (file.isDirectory()) {
    throw new IllegalStateException("The folder: " + file + " must not exist before tests are run.");
  }
}

return true;
