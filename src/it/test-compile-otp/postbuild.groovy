File target = new File(basedir, "target/test-test-compile-otp-0/ebin/some_test.beam");
if (!target.isFile()) {
    throw new IllegalStateException("The compiled test target " + target + " was missing.");
}