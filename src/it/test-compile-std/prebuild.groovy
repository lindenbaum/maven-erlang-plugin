File target = new File(basedir, "target/test-test-compile-std-0/ebin/some_test.beam");
if (target.isFile()) {
    throw new IllegalStateException("The compiled test target " + target + " must not exist before test is run.");
}