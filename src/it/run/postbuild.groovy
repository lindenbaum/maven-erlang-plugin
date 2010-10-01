File target = new File(basedir, "target/ebin/touched");
if (!target.isFile()) {
    throw new IllegalStateException("Target touch file " + target + " was missing, should have been created by started application.");
}
