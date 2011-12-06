File edoc = new File(basedir, "src/site/overview.edoc");
if (!edoc.isFile()) {
    throw new IllegalStateException("Required file " + edoc + " was missing before test was run.");
}
