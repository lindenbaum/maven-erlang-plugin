File app1 = new File(basedir, "app_1/target/app_1-0.tar.gz");
File app2 = new File(basedir, "app_2/target/app_2-0.tar.gz");
File app3 = new File(basedir, "app_3/target/app_3-0.tar.gz");

if (!app1.isFile()) {
  throw new IllegalStateException("The module application " + app1 + " was missing.");
}

if (!app2.isFile()) {
  throw new IllegalStateException("The module application " + app2 + " was missing.");
}

if (!app3.isFile()) {
  throw new IllegalStateException("The module application " + app3 + " was missing.");
}

File dep = new File(basedir, "app_1/target/lib/app_2-0");
if (!dep.isDirectory()) {
  throw new IllegalStateException("The module application dependency " + dep + " was not resolved.");
}