File target = new File(basedir, "touched");
if (target.isFile()) {
  throw new IllegalStateException("Target touch file " + target + " must not exists before tests are run.");
}
