package eu.lindenbaum.maven;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

import eu.lindenbaum.maven.util.CollectionUtils;
import eu.lindenbaum.maven.util.CollectionUtils.FoldFunction;
import eu.lindenbaum.maven.util.CollectionUtils.MapFunction;
import eu.lindenbaum.maven.util.ErlConstants;
import eu.lindenbaum.maven.util.FileUtils;
import eu.lindenbaum.maven.util.MavenUtils;
import eu.lindenbaum.maven.util.NetworkUtils;

import org.apache.maven.artifact.Artifact;
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
  public List<File> codePaths(boolean withTestCodePaths) {
    List<File> paths = FileUtils.getDirectoriesRecursive(this.targetLayout.lib(), ErlConstants.BEAM_SUFFIX);
    switch (this.packagingType) {
      case ERLANG_OTP:
      case ERLANG_STD: {
        paths.add(withTestCodePaths ? this.targetLayout.testEbin() : this.targetLayout.ebin());
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
  public List<File> includePaths(boolean withTestIncludePaths) {
    List<File> deps = FileUtils.getDirectoriesRecursive(this.targetLayout.lib(), ErlConstants.HRL_SUFFIX);
    HashSet<File> paths = new HashSet<File>(deps);
    if (this.packagingType != PackagingType.ERLANG_REL) {
      if (withTestIncludePaths) {
        paths.addAll(this.sourceLayout.testSrcs());
        paths.add(this.sourceLayout.testInclude());
      }
      paths.add(this.sourceLayout.src());
      paths.add(this.sourceLayout.include());
    }
    return new ArrayList<File>(paths);
  }

  @Override
  public List<File> testSupportScripts() {
    List<File> files = new ArrayList<File>();
    File testEbin = this.targetLayout.testEbin();
    files.add(new File(testEbin, "surefire.erl"));
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

  @Override
  public List<File> modules(boolean withTests, boolean withDependencies) {
    File ebin = withTests ? this.targetLayout.testEbin() : this.targetLayout.ebin();
    List<File> modules = FileUtils.getFilesRecursive(ebin, ErlConstants.BEAM_SUFFIX);
    if (withDependencies) {
      modules.addAll(dependencyModules(withTests));
    }
    return modules;
  }

  @Override
  public List<File> dependencyModules(boolean withTestScopeDependencies) {
    List<File> dependencies = getDependencyDirectories(withTestScopeDependencies);
    return FileUtils.getFilesRecursive(dependencies, ErlConstants.BEAM_SUFFIX);
  }

  @Override
  public List<File> applicationFiles(boolean withDependencies) {
    List<File> applicationFiles = new ArrayList<File>();
    switch (this.packagingType) {
      case ERLANG_OTP:
      case ERLANG_STD: {
        applicationFiles.add(this.targetLayout.appFile());
        break;
      }
      default: {
        break;
      }
    }
    if (withDependencies) {
      List<File> dependencies = getDependencyDirectories(false);
      applicationFiles.addAll(FileUtils.getFilesRecursive(dependencies, ErlConstants.APP_SUFFIX));
    }
    return applicationFiles;
  }

  @Override
  public List<File> resources(boolean withTests, boolean withDependencies) {
    final File priv = withTests ? this.targetLayout.testPriv() : this.targetLayout.priv();
    ArrayList<File> privDirectories = new ArrayList<File>(Arrays.asList(priv));
    if (withDependencies) {
      privDirectories.addAll(CollectionUtils.map(new MapFunction<File, File>() {
        @Override
        public File apply(File a) {
          return new File(a, priv.getName());
        }
      }, getDependencyDirectories(withTests)));
    }
    return FileUtils.getFilesRecursive(privDirectories, "");
  }

  private List<File> getDependencyDirectories(final boolean withTestScopeDependencies) {
    final File lib = this.targetLayout.lib();
    return CollectionUtils.foldl(new FoldFunction<Artifact, List<File>>() {
      @Override
      public List<File> apply(Artifact artifact, List<File> acc) {
        if (withTestScopeDependencies || !Artifact.SCOPE_TEST.equals(artifact.getScope())) {
          acc.add(new File(lib, MavenUtils.getArtifactDirectory(artifact)));
        }
        return acc;
      }
    }, new ArrayList<File>(), MavenUtils.getErlangArtifacts(this.project));
  }
}