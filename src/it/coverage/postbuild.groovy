File target = new File(basedir, "target/site/erlang-coverage-report.html");
if (!target.isFile()) {
    throw new IllegalStateException("The coverage report " + target + " was missing.");
}