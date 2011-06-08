File target = new File(basedir, "target/compile_otp-0/ebin/plugin_compile.beam");
if (target.isFile()) {
    throw new IllegalStateException("The compiled target " + target + " must not exist before test is run.");
}