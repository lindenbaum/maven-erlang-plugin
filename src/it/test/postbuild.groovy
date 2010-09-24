File coverage = new File(basedir, "target/test/coverdata.coverdata");
if (!coverage.isFile()) {
    throw new IllegalStateException("Coverdata file " + coverage + " was missing.");
}

File report = new File(basedir, "target/surefire/TEST-test_server_test.xml");
if (!report.isFile()) {
    throw new IllegalStateException("Test report file " + report + " was missing.");
}
