File doc = new File(basedir, "target/site/edoc/index.html");
if (!doc.isFile()) {
    throw new IllegalStateException("EDoc index page " + doc + " was missing.");
}

File edoc = new File(basedir, "target/overview.edoc");
if (!edoc.isFile()) {
    throw new IllegalStateException("Overview file " + edoc + " was missing.");
}
