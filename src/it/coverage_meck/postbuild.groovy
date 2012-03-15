File target = new File(basedir, "target/erlang-coverage-report.html");
if (!target.isFile()) {
  throw new IllegalStateException("The coverage report " + target + " was missing.");
}

File reportFile = new File(basedir, "target/coverage-reports/COVERAGE-coverage_meck.txt");
if (!reportFile.isFile()) {
  throw new IllegalStateException("The CSV report file " + reportFile + " was missing.");
}
