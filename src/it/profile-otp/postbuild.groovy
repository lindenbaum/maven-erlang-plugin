File dir = new File(basedir, "target/profiling-reports");
if (!dir.isDirectory()) {
    throw new IllegalStateException("Profiling report directory " + dir + " was missing.");
}

File report = new File(basedir, "target/profiling-reports/PROFILING-profile_otp.txt");
if (!report.isFile()) {
    throw new IllegalStateException("Profiling report " + report + " is missing.");
}
