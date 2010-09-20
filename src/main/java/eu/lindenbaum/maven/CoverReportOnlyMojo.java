package eu.lindenbaum.maven;

/**
 * Generate the cover report only (do not run tests). This requires that tests were previously run with
 * codeCoverage option set to true.
 * 
 * @goal cover-report-only
 * @execute phase="validate" lifecycle="cover-report"
 */
public final class CoverReportOnlyMojo extends CoverReportMojo {
}
