File parser = new File(basedir, "target/yecc_std-0/ebin/html.beam");
if (parser.isFile()) {
    throw new IllegalStateException("Parser " + parser + " must not exist before test is run.");
}
