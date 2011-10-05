File target = new File(basedir, "target/erlang-coverage-report.html");
if (target.isFile()) {
  throw new IllegalStateException("The coverage report " + target + " must not exist before test is run.");
}

File skipped = new File(basedir, "src/mod_5.erl");
if (!skipped.isFile()) {
  throw new IllegalStateException("The module " + skipped + " to exclude must not be missing before test is run.");
}
