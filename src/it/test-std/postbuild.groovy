File report1 = new File(basedir, "target/surefire-reports/TEST-test_server_test.xml");
if (!report1.isFile()) {
    throw new IllegalStateException("Test report file " + report1 + " was missing.");
}

File report2 = new File(basedir, "target/surefire-reports/TEST-test_app_test.xml");
if (!report2.isFile()) {
    throw new IllegalStateException("Test report file " + report2 + " was missing.");
}
