File doc = new File(basedir, "target/site/doc/index.html");
if (doc.isFile()) {
    throw new IllegalStateException("EDoc index page " + doc + " must not exist before test is run.");
}

File testDoc = new File(basedir, "target/site/test-doc/index.html");
if (testDoc.isFile()) {
    throw new IllegalStateException("Test EDoc index page " + testDoc + " must not exist before test is run.");
}