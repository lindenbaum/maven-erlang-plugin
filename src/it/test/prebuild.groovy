File coverage = new File(basedir, "target/test/coverdata.coverdata");
if (coverage.isFile()) {
    throw new IllegalStateException("Coverdata file " + coverage + " must not exist before test is run.");
}

File report1 = new File(basedir, "target/surefire-reports/TEST-test_server_test.xml");
if (report1.isFile()) {
    throw new IllegalStateException("Test report file " + report1 + " must not exist before test is run.");
}

File report2 = new File(basedir, "target/surefire-reports/TEST-test_app_test.xml");
if (report2.isFile()) {
    throw new IllegalStateException("Test report file " + report2 + " must not exist before test is run.");
}
