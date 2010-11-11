File target = new File(basedir, "target/ebin/touched");
if (!target.isFile()) {
    throw new IllegalStateException("Target touch file " + target + " was missing, should have been created by started application.");
}

String node = "defined"

target.eachLine {
  if (!it.contains(node)) {
    throw new IllegalStateException("Touched file was expected to contain node name: ${node}")
  }
}

return true