File target = new File(basedir, "target/erlang-coverage-report.html");
if (!target.isFile()) {
  throw new IllegalStateException("The coverage report " + target + " was missing.");
}

String text = target.getText();
if (text.indexOf("summary: 50,") == -1) {
  throw new IllegalStateException("Could not verify expected coverage of 50%, please check " + target + " for actual value.");
}

File reportFile = new File(basedir, "target/coverage-reports/COVERAGE-coverage_otp.txt");
if (!reportFile.isFile()) {
  throw new IllegalStateException("The CSV report file " + reportFile + " was missing.");
}

reportFile.eachLine { line ->
  if (line.indexOf("mod_5") >= 0) {
    throw new IllegalStateException("The excluded module [mod_5] was covered, but should not be.");
  }
}
