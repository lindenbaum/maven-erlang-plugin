File target = new File(basedir, "target/ebin/plugin_compile.beam");
if (!target.isFile()) {
    throw new IllegalStateException("The compiled target " + target + " was missing.");
}