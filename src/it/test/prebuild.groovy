File coverage = new File(basedir, "target/test/coverdata.coverdata");
if (coverage.isFile()) {
    throw new IllegalStateException("Coverdata file " + coverage + " must not exist before test is run.");
}

File report = new File(basedir, "target/surefire/TEST-test_server_test.xml");
if (report.isFile()) {
    throw new IllegalStateException("Test report file " + report + " must not exist before test is run.");
}
