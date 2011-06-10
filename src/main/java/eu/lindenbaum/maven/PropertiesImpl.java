package eu.lindenbaum.maven;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.NetworkUtils;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;

/**
 * Implementation of the {@link Properties} bean that provides the directory
 * layout as described in {@link PackagingType}.
 * 
 * @see PackagingType
 */
final class PropertiesImpl implements Properties {
  /**
   * Name of the default backend node used by the plug-in.
   */
  private static final String DEFAULT_BACKEND;

  /**
   * Name of the default test backend node used by the plug-in.
   */
  private static final String DEFAULT_TEST_BACKEND;

  static {
    String iPv4Address = "";
    try {
      iPv4Address = "@" + NetworkUtils.getIPv4Address();
    }
    catch (MojoExecutionException e) {
      System.out.println("[WARN] " + e.getMessage());
    }
    DEFAULT_BACKEND = "maven-erlang-plugin-backend" + iPv4Address;
    DEFAULT_TEST_BACKEND = "maven-erlang-plugin-test-backend" + iPv4Address;
  }

  private final MavenProject project;

  private final MavenComponents components;
  private final PackagingType packagingType;
  private final String erlCommand;
  private final String node;
  private final String testNode;
  private final String cookie;

  private final TargetLayout targetLayout;
  private final SourceLayout sourceLayout;

  PropertiesImpl(PackagingType type,
                 MavenProject project,
                 MavenComponents components,
                 String erlCommand,
                 String cookie) {
    this.project = project;
    this.components = components;

    this.packagingType = type;
    this.erlCommand = erlCommand;
    this.node = project.getArtifactId() + "-" + DEFAULT_BACKEND;
    this.testNode = project.getArtifactId() + "-" + DEFAULT_TEST_BACKEND;
    this.cookie = cookie;

    this.targetLayout = new DefaultTargetLayout(project);
    switch (type) {
      case ERLANG_STD: {
        this.sourceLayout = new ErlangStdLayout(project);
        break;
      }
      case ERLANG_OTP: {
        this.sourceLayout = new ErlangOtpLayout(project);
        break;
      }
      default: {
        this.sourceLayout = new ErlangRelLayout(project);
        break;
      }
    }
  }

  @Override
  public MavenProject project() {
    return this.project;
  }

  @Override
  public MavenComponents components() {
    return this.components;
  }

  @Override
  public PackagingType packagingType() {
    return this.packagingType;
  }

  @Override
  public String erlCommand() {
    return this.erlCommand;
  }

  @Override
  public String node() {
    return this.node;
  }

  @Override
  public String testNode() {
    return this.testNode;
  }

  @Override
  public String cookie() {
    return this.cookie;
  }

  @Override
  public SourceLayout sourceLayout() {
    return this.sourceLayout;
  }

  @Override
  public TargetLayout targetLayout() {
    return this.targetLayout;
  }

  @Override
  public List<File> codePaths() {
    List<File> paths = FileUtils.getDirectoriesRecursive(this.targetLayout.lib(), ErlConstants.BEAM_SUFFIX);
    switch (this.packagingType) {
      case ERLANG_OTP:
      case ERLANG_STD: {
        paths.add(this.targetLayout.ebin());
        break;
      }
      default: {
        paths.add(this.targetLayout.base());
        break;
      }
    }
    return paths;
  }

  @Override
  public List<File> testCodePaths() {
    List<File> paths = FileUtils.getDirectoriesRecursive(this.targetLayout.lib(), ErlConstants.BEAM_SUFFIX);
    switch (this.packagingType) {
      case ERLANG_OTP:
      case ERLANG_STD: {
        paths.add(this.targetLayout.testEbin());
        break;
      }
      default: {
        paths.add(this.targetLayout.base());
        break;
      }
    }
    return paths;
  }

  @Override
  public List<File> includePaths() {
    List<File> paths = FileUtils.getDirectoriesRecursive(this.targetLayout.lib(), ErlConstants.HRL_SUFFIX);
    if (this.packagingType != PackagingType.ERLANG_REL) {
      paths.add(this.sourceLayout.src());
      paths.add(this.sourceLayout.include());
    }
    return paths;
  }

  @Override
  public List<File> testIncludePaths() {
    List<File> paths = FileUtils.getDirectoriesRecursive(this.targetLayout.lib(), ErlConstants.HRL_SUFFIX);
    if (this.packagingType != PackagingType.ERLANG_REL) {
      paths.addAll(this.sourceLayout.testSrcs());
      paths.add(this.sourceLayout.include());
      paths.add(this.sourceLayout.testInclude());
    }
    return paths;
  }

  @Override
  public List<File> testSupportScripts() {
    List<File> files = new ArrayList<File>();
    File testEbin = this.targetLayout.testEbin();
    files.add(new File(testEbin, "surefire.erl"));
    files.add(new File(testEbin, "cover2.erl"));
    files.add(new File(testEbin, "ttycapture.erl"));
    return files;
  }

  @Override
  public List<File> testSupportArtifacts() {
    List<File> artifacts = new ArrayList<File>();
    for (File script : testSupportScripts()) {
      String erl = ErlConstants.ERL_SUFFIX;
      String beam = ErlConstants.BEAM_SUFFIX;
      artifacts.add(new File(script.getAbsolutePath().replace(erl, beam)));
    }
    return artifacts;
  }
}