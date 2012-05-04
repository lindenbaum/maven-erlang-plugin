File parser = new File(basedir, "target/yecc_otp-0/ebin/html.beam");
if (!parser.isFile()) {
    throw new IllegalStateException("Parser " + parser + " is missing.");
}
