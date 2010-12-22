[
  new File(basedir, "src/site/site.xml"),
  new File(basedir, "src/site/apt/index.apt.vm"),
  new File(basedir, "src/changes/changes.xml"),
  new File(basedir, "src/main/erlang/setup-otp.app"),
  new File(basedir, "src/main/erlang/setup-otp.appup")
].each { file ->
  if (file.isFile()) {
    throw new IllegalStateException("The target file: " + file + " must not exist before test is run.");
  }
}

[
  new File(basedir, "src/main/erlang"),
  new File(basedir, "src/main/include"),
  new File(basedir, "src/main/priv"),  
  new File(basedir, "src/test/erlang"),
  new File(basedir, "src/test/include")
].each { dir ->
  if (dir.isDirectory()) {
    throw new IllegalStateException("The target directory: " + dir + " must not exist before test is run.");
  }
}
    
return true;
