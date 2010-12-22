File target = new File(basedir, "target/erlang-coverage-report.html");
if (!target.isFile()) {
    throw new IllegalStateException("The coverage report " + target + " was missing.");
}