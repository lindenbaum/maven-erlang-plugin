File coverage = new File(basedir, "target/test/coverdata.coverdata");
if (!coverage.isFile()) {
    throw new IllegalStateException("Coverdata file " + coverage + " was missing.");
}

File report1 = new File(basedir, "target/surefire-reports/TEST-test_server_test.xml");
if (!report1.isFile()) {
    throw new IllegalStateException("Test report file " + report1 + " was missing.");
}

File report2 = new File(basedir, "target/surefire-reports/TEST-test_app_test.xml");
if (!report2.isFile()) {
    throw new IllegalStateException("Test report file " + report2 + " was missing.");
}

File resourceFile = new File(basedir, "target/test/resource-file");
if (!resourceFile.isFile()) {
    throw new IllegalStateException("Test resource file " + resourceFile + " was missing.");
}

File resourcePrivFile = new File(basedir, "target/test/resources/priv/resource-priv-file");
if (resourcePrivFile.isFile()) {
    throw new IllegalStateException("Test resource priv file " + resourcePrivFile + " should have been filtered.");
}    