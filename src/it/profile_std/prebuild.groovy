File dir = new File(basedir, "target/profiling-reports");
if (dir.isDirectory()) {
    throw new IllegalStateException("Profiling report directory " + dir + " must not exist before test is run.");
}

File report = new File(basedir, "target/profiling-reports/PROFILING-profile_std.txt");
if (report.isFile()) {
    throw new IllegalStateException("Profiling report " + report + " must not exist before test is run.");
}
