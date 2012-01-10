File target = new File(basedir, "target/compile_otp-0/ebin/plugin_compile.beam");
if (!target.isFile()) {
  throw new IllegalStateException("The compiled target BEAM file " + target + " was missing.");
}

File mib = new File(basedir, "target/compile_otp-0/priv/GOOD-MIB.bin");
if (!mib.isFile()) {
  throw new IllegalStateException("The compiled target MIB file " + target + " was missing.");
}
