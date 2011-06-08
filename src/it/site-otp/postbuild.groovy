[
  new File(basedir, "target/profiling-reports"),
  new File(basedir, "target/coverage-reports")
].each { file ->
  if (!file.isDirectory()) {
    throw new IllegalStateException("The target folder: " + file + " should have been created by the site goal.");
  }
}

return true;