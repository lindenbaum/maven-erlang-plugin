File target = new File(basedir, "target/site/coverage/erlang-coverage-report.html");
if (target.isFile()) {
    throw new IllegalStateException("The coverage report " + target + " must not exist before test is run.");
}