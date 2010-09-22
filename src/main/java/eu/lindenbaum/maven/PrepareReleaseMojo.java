package eu.lindenbaum.maven;

import static eu.lindenbaum.maven.util.ErlUtils.eval;
import static eu.lindenbaum.maven.util.FileUtils.getDependencies;

import java.io.File;
import java.io.FileFilter;
import java.io.FilenameFilter;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DefaultArtifact;
import org.apache.maven.artifact.repository.ArtifactRepository;
import org.apache.maven.artifact.versioning.VersionRange;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.project.MavenProject;

/**
 * Prepare the release.
 * 
 * @goal prepare-release
 * @phase compile
 */
public final class PrepareReleaseMojo extends AbstractMojo {
  /**
   * Command to extract the version from the .rel file.
   */
  private static final String EXTRACT_VERSION = "{ok, [{release, {_ReleaseName, ReleaseVersion}, {erts, _ErtsVersion}, _Applications}]} = file:consult(\"%s\"), "
                                                + "io:format(ReleaseVersion), io:nl().";

  /**
   * Command to extract the applications from the .app file.
   */
  private static final String EXTRACT_APPLICATIONS = "{ok, [{release, {_ReleaseName, _ReleaseVersion}, {erts, _ErtsVersion}, Applications}]} = file:consult(\"%s\"), "
                                                     + "lists:foreach(fun(Tuple) when is_tuple(Tuple) -> AppName = element(1, Tuple), AppVersion = element(2, Tuple), io:format(\"~p ~s~n\", [AppName, AppVersion]) end, Applications).";

  /**
   * Project to interact with.
   * 
   * @parameter expression="${project}"
   * @required
   * @readonly
   */
  private MavenProject project;

  /**
   * Directory where the boot and script files are created.
   * 
   * @parameter expression="${project.build.directory}/ebin/"
   */
  private File outputDirectory;

  /**
   * Source directory.
   * 
   * @parameter expression="${basedir}/src/main/erlang/"
   */
  private File inputDirectory;

  /**
   * Directories where dependencies are unpacked. This directory contains OTP applications (name-version
   * directories, with include and ebin sub directories).
   * 
   * @parameter expression="${project.build.directory}/lib/"
   * @required
   */
  private File libDirectory;

  /**
   * Name of the release. Defaults to the artifact id.
   * 
   * @parameter
   */
  private String releaseName;

  /**
   * Options for make_script.
   * 
   * @parameter
   */
  private String[] makeScriptOptions;

  /**
   * Options for make_relup.
   * 
   * @parameter
   */
  private String[] makeRelupOptions;

  /**
   * Options for the relup up from and down to.
   * 
   * @parameter default-value="${relupVersion}"
   */
  private String relupVersion;

  /**
   * Options for the relup up from.
   * 
   * @parameter default-value="${relupUpFrom}"
   */
  private String relupUpFrom;

  /**
   * Option for the relup down to.
   * 
   * @parameter default-value="${relupDownTo}"
   */
  private String relupDownTo;

  /**
   * Directories where releases are unpacked.
   * 
   * @parameter expression="${project.build.directory}/other_releases/"
   */
  private File releasesDirectory;

  /**
   * @parameter expression="${localRepository}"
   * @required
   * @readonly
   */
  private ArtifactRepository localRepository;

  /**
   * Version of the project.
   * 
   * @parameter expression="${project.version}"
   */
  private String version;

  /*
   * Mojo interface.
   */
  public void execute() throws MojoExecutionException, MojoFailureException {
    // First prepare the release file.
    if (!this.outputDirectory.exists()) {
      this.outputDirectory.mkdirs();
    }

    final Map<String, String> theApplications = getApplicationDependencies();
    copyReleaseFile();
    checkVersion();
    checkApplicationVersions(theApplications);

    // Determine if we're building a relup file.
    if (this.relupUpFrom == null) {
      this.relupUpFrom = this.relupVersion;
    }
    if (this.relupDownTo == null) {
      this.relupDownTo = this.relupVersion;
    }
    // Determine if we're building a relup.
    if ((this.relupUpFrom != null) && (this.relupDownTo != null)) {
      makeRelup();
    }

    // Make the script.
    makeScript();
  }

  /**
   * Copy the release file and perform substitutions.
   */
  private void copyReleaseFile() throws MojoExecutionException {
    Map<String, String> replacements = new HashMap<String, String>();
    replacements.put("\\?REL_VERSION", "\"" + this.project.getVersion() + "\"");

    final String name = getReleaseName() + ErlConstants.REL_SUFFIX;
    FileUtils.copyDirectory(this.inputDirectory, this.outputDirectory, new FileFilter() {
      @Override
      public boolean accept(File pathname) {
        return pathname.getName().equals(name);
      }
    }, replacements);
  }

  /**
   * Invoke make_relup.
   */
  private void makeRelup() throws MojoExecutionException, MojoFailureException {
    if (!this.outputDirectory.exists()) {
      this.outputDirectory.mkdirs();
    }

    final File[] theReleaseDirs;
    // Extract the previous releases.
    if (this.relupUpFrom.equals(this.relupDownTo)) {
      theReleaseDirs = unpackReleases(new String[]{ this.relupUpFrom });
    }
    else {
      theReleaseDirs = unpackReleases(new String[]{ this.relupUpFrom, this.relupDownTo });
    }

    @SuppressWarnings("unused")
    final String theArtifactId = this.project.getArtifactId();
    final File theRelFile = getReleaseFile();

    final StringBuilder theMakeRelupLineBuffer = new StringBuilder();
    theMakeRelupLineBuffer.append("Status = systools:make_relup(\"");
    theMakeRelupLineBuffer.append(theRelFile.getPath());
    theMakeRelupLineBuffer.append("\"");

    final File theUpFromRelFile = getRelFile(new File(theReleaseDirs[0], "releases"));
    final File theDownToRelFile;
    if (theReleaseDirs.length > 1) {
      theDownToRelFile = getRelFile(new File(theReleaseDirs[1], "releases"));
    }
    else {
      theDownToRelFile = theUpFromRelFile;
    }
    theMakeRelupLineBuffer.append(", [\"");
    theMakeRelupLineBuffer.append(theUpFromRelFile.getPath());
    theMakeRelupLineBuffer.append("\"]");

    theMakeRelupLineBuffer.append(", [\"");
    theMakeRelupLineBuffer.append(theDownToRelFile.getPath());
    theMakeRelupLineBuffer.append("\"]");

    theMakeRelupLineBuffer.append(", [");
    theMakeRelupLineBuffer.append("{outdir, \"").append(this.outputDirectory.getPath()).append("\"}, ");
    theMakeRelupLineBuffer.append("{path, [");
    boolean firstDir = true;
    for (File theReleaseDir : theReleaseDirs) {
      if (!firstDir) {
        theMakeRelupLineBuffer.append(",");
      }
      else {
        firstDir = false;
      }
      theMakeRelupLineBuffer.append("\"").append(theReleaseDir.getPath()).append("/lib/*/ebin\"");
    }
    theMakeRelupLineBuffer.append("]}");
    if (this.makeRelupOptions != null) {
      for (String theOption : this.makeRelupOptions) {
        theMakeRelupLineBuffer.append(", ");
        theMakeRelupLineBuffer.append(theOption);
      }
    }
    theMakeRelupLineBuffer.append("]");

    theMakeRelupLineBuffer.append("), StatusCode = case Status of ok -> 0; _ -> 1 end, erlang:halt(StatusCode).");
    String theResult = eval(getLog(), theMakeRelupLineBuffer.toString(), getDependencies(this.libDirectory));
    // Print any warning.
    if (!"".equals(theResult)) {
      getLog().info(theResult);
    }
  }

  /**
   * Invoke make_script.
   */
  private void makeScript() throws MojoExecutionException, MojoFailureException {
    @SuppressWarnings("unused")
    final String theReleaseName;
    if (this.releaseName == null) {
      theReleaseName = this.project.getArtifactId();
    }
    else {
      theReleaseName = this.releaseName;
    }

    final File theRelFile = getReleaseFile();

    final StringBuilder theMakeScriptLineBuffer = new StringBuilder();
    theMakeScriptLineBuffer.append("Status = systools:make_script(\"");
    theMakeScriptLineBuffer.append(theRelFile.getPath());
    theMakeScriptLineBuffer.append("\"");

    theMakeScriptLineBuffer.append(", [");
    theMakeScriptLineBuffer.append("{outdir, \"").append(this.outputDirectory.getPath()).append("\"}");
    if (this.makeScriptOptions != null) {
      for (String theOption : this.makeScriptOptions) {
        theMakeScriptLineBuffer.append(", ");
        theMakeScriptLineBuffer.append(theOption);
      }
    }
    theMakeScriptLineBuffer.append("]");

    theMakeScriptLineBuffer.append("), StatusCode = case Status of ok -> 0; _ -> 1 end, erlang:halt(StatusCode).");
    String theResult = eval(getLog(), theMakeScriptLineBuffer.toString(), getDependencies(this.libDirectory));
    // Print any warning.
    if (!"".equals(theResult)) {
      getLog().info(theResult);
    }
  }

  /**
   * Unpack releases inside the other_releases directory.
   * 
   * @throws MojoExecutionException if there was a problem with unpacking the dependencies.
   */
  private File[] unpackReleases(String[] inReleases) throws MojoExecutionException, MojoFailureException {
    if (!this.releasesDirectory.exists()) {
      this.releasesDirectory.mkdirs();
    }

    final Artifact theArtifact = this.project.getArtifact();
    final String theArtifactId = theArtifact.getArtifactId();
    final List<File> thePaths = new LinkedList<File>();

    for (String theRelease : inReleases) {
      final Artifact theOtherReleaseArtifact = new DefaultArtifact(theArtifact.getGroupId(),
                                                                   theArtifactId,
                                                                   VersionRange.createFromVersion(theRelease),
                                                                   theArtifact.getScope(),
                                                                   theArtifact.getType(),
                                                                   theArtifact.getClassifier(),
                                                                   theArtifact.getArtifactHandler());
      final String localPath = this.localRepository.pathOf(theOtherReleaseArtifact);
      final File theArtifactFile = new File(this.localRepository.getBasedir(), localPath);
      final String theReleaseName = theArtifactId + "-" + theRelease;
      if (!theArtifactFile.exists()) {
        throw new MojoExecutionException("Artifact " + theReleaseName
                                         + " could not be found in the local repository");
      }
      final File theReleaseExtractDir = new File(this.releasesDirectory, theReleaseName);
      theReleaseExtractDir.mkdirs();
      thePaths.add(theReleaseExtractDir);

      // Plexus TarGZipUnarchiver is broken, so we do that in erlang.

      final StringBuilder theUntarLineBuffer = new StringBuilder();
      theUntarLineBuffer.append("Status = erl_tar:extract(\"");
      theUntarLineBuffer.append(theArtifactFile.getPath());
      theUntarLineBuffer.append("\", [{cwd, \"");
      theUntarLineBuffer.append(theReleaseExtractDir.getPath());
      theUntarLineBuffer.append("\"}, compressed]");
      theUntarLineBuffer.append("), StatusCode = case Status of ok -> 0; _ -> 1 end, erlang:halt(StatusCode).");
      String theResult = eval(getLog(), theUntarLineBuffer.toString(), getDependencies(this.libDirectory));
      // Print any warning.
      if (!"".equals(theResult)) {
        getLog().info(theResult);
      }

    }

    return thePaths.toArray(new File[0]);
  }

  /**
   * Get the .rel file for an older release.
   */
  private File getRelFile(File inRelDir) {
    final File[] theReleaseFiles = inRelDir.listFiles(new FilenameFilter() {
      public boolean accept(File inDir, String inName) {
        return inName != null && inName.endsWith(ErlConstants.REL_SUFFIX);
      }
    });
    if (theReleaseFiles.length != 1) {
      return new File(inRelDir, this.releaseName);
    }
    final String theSingleRelFileName = theReleaseFiles[0].getName();
    final String theReleaseName = theSingleRelFileName.substring(0, theSingleRelFileName.length()
                                                                    - ErlConstants.REL_SUFFIX.length());
    return new File(inRelDir, theReleaseName);
  }

  /**
   * Get the release name. It's variable releaseName or the artifact id if it's not set.
   */
  private String getReleaseName() {
    final String theReleaseName;
    if (this.releaseName == null) {
      theReleaseName = this.project.getArtifactId();
    }
    else {
      theReleaseName = this.releaseName;
    }

    return theReleaseName;
  }

  /**
   * Get the release file. It's situated in the output directory (it was copied or compiled from a .rel.src
   * file).
   */
  private File getReleaseFile() {
    return new File(this.outputDirectory, getReleaseName());
  }

  /**
   * Check the version of the release file, it must match the pom's version.
   */
  private void checkVersion() throws MojoExecutionException, MojoFailureException {
    final File theRelFile = new File(this.outputDirectory, getReleaseName() + ErlConstants.REL_SUFFIX);

    // Check the version.
    final String theCheckVersionExpr = String.format(EXTRACT_VERSION, theRelFile.getPath());
    final String theRelVersion = eval(getLog(), theCheckVersionExpr, getDependencies(this.libDirectory));
    if (!this.version.equals(theRelVersion)) {
      getLog().error("Version mismatch. Project version is " + this.version + " while .rel version is "
                     + theRelVersion);
      throw new MojoFailureException("Version mismatch");
    }
  }

  /**
   * Check the version of the applications in the release, it must match the dependencies versions.
   */
  private void checkApplicationVersions(Map<String, String> inApplications) throws MojoExecutionException,
                                                                           MojoFailureException {
    final File theRelFile = new File(this.outputDirectory, getReleaseName() + ErlConstants.REL_SUFFIX);

    // Extract the application versions
    final String theCheckVersionExpr = String.format(EXTRACT_APPLICATIONS, theRelFile.getPath());
    final String[] theAppVersions = eval(getLog(), theCheckVersionExpr, getDependencies(this.libDirectory)).split("\\n");
    for (String theAppVersion : theAppVersions) {
      final String[] theApp = theAppVersion.split(" ");
      if (theApp.length != 2) {
        throw new MojoFailureException("cannot parse the app version " + theAppVersion);
      }
      final String depVersion = inApplications.get(theApp[0]);
      if ((depVersion != null) && !depVersion.equals(theApp[1])) {
        getLog().error("Version mismatch. Application dependency version is " + depVersion
                       + " while the version in the .rel file is " + theApp[1]);
        throw new MojoFailureException("Application version mismatch");
      }
    }
  }

  /**
   * Get the application dependencies.
   */
  @SuppressWarnings("unchecked")
  private Map<String, String> getApplicationDependencies() {
    final Map<String, String> theResult = new HashMap<String, String>();
    final Set<Artifact> theArtifacts = this.project.getArtifacts();
    for (Artifact theArtifact : theArtifacts) {
      if (theArtifact.getType().equals(ErlConstants.ARTIFACT_TYPE_OTP)) {
        theResult.put(theArtifact.getArtifactId(), theArtifact.getVersion());
      }
    }

    return theResult;
  }
}
