File doc = new File(basedir, "target/site/edoc/index.html");
if (doc.isFile()) {
    throw new IllegalStateException("EDoc index page " + doc + " must not exist before test is run.");
}

File edoc = new File(basedir, "target/overview.edoc");
if (edoc.isFile()) {
    throw new IllegalStateException("Overview file " + edoc + " must not exist before test is run.");
}
