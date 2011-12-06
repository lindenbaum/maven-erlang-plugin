File edoc = new File(basedir, "doc/overview.edoc");
if (!edoc.isFile()) {
    throw new IllegalStateException("Required file " + edoc + " was missing before test was run.");
}
