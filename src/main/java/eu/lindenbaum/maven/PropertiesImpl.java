package eu.lindenbaum.maven;

import java.io.File;

import eu.lindenbaum.maven.util.ErlConstants;

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
  private static final String DEFAULT_BACKEND = "maven-erlang-plugin-backend";

  /**
   * Name of the default test backend node used by the plug-in.
   */
  private static final String DEFAULT_TEST_BACKEND = "maven-erlang-plugin-test-backend";

  private final MavenProject project;

  private final String projectName;
  private final MavenComponents components;
  private final PackagingType packagingType;
  private final String erlCommand;
  private final String node;
  private final String testNode;
  private final String cookie;

  private final File apt;
  private final File base;
  private final File changes;
  private final File ebin;
  private final File include;
  private final File priv;
  private final File site;
  private final File src;
  private final File src_base;
  private final File test_include;
  private final File test_priv;
  private final File test_src;

  private final File target;
  private final File targetEbin;
  private final File targetInclude;
  private final File targetLib;
  private final File targetMibs;
  private final File targetPriv;
  private final File targetProject;
  private final File targetReleases;
  private final File targetSite;
  private final File targetSrc;
  private final File targetSurefireReports;
  private final File targetTest;
  private final File targetTestEbin;
  private final File targetTestPriv;

  PropertiesImpl(PackagingType type,
                 MavenProject project,
                 MavenComponents components,
                 File base,
                 File target,
                 String erlCommand,
                 String cookie) {
    this.project = project;
    this.components = components;

    this.projectName = project.getArtifactId() + "-" + project.getVersion();
    this.packagingType = type;
    this.erlCommand = erlCommand;
    this.node = DEFAULT_BACKEND;
    this.testNode = DEFAULT_TEST_BACKEND;
    this.cookie = cookie;

    switch (type) {
      case ERLANG_STD: {
        this.apt = new File(base, "src/site/apt");
        this.changes = new File(base, "src/changes");
        this.ebin = new File(base, "ebin");
        this.include = new File(base, "include");
        this.priv = new File(base, "priv");
        this.site = new File(base, "src/site");
        this.src = new File(base, "src");
        this.src_base = base;
        this.test_include = new File(base, "test_include");
        this.test_priv = new File(base, "test_priv");
        this.test_src = new File(base, "test_src");
        break;
      }
      case ERLANG_OTP: {
        this.apt = new File(base, "src/site/apt");
        this.changes = new File(base, "src/changes");
        this.ebin = new File(base, "src/main/erlang");
        this.include = new File(base, "src/main/include");
        this.priv = new File(base, "src/main/priv");
        this.site = new File(base, "src/site");
        this.src = this.ebin;
        this.src_base = new File(base, "src/main");
        this.test_include = new File(base, "src/test/include");
        this.test_priv = new File(base, "src/test/priv");
        this.test_src = new File(base, "src/test/erlang");
        break;
      }
      default: { // ERLANG_REL
        this.apt = new File(base, "src/site/apt");
        this.changes = new File(base, "src/changes");
        this.ebin = base;
        this.include = base;
        this.priv = base;
        this.site = new File(base, "src/site");
        this.src = base;
        this.src_base = base;
        this.test_include = base;
        this.test_priv = base;
        this.test_src = base;
        break;
      }
    }

    this.base = base;
    this.target = target;
    this.targetProject = new File(this.target, this.projectName);
    this.targetEbin = new File(this.targetProject, "ebin");
    this.targetInclude = new File(this.targetProject, "include");
    this.targetLib = new File(this.target, "lib");
    this.targetMibs = new File(this.targetProject, "mibs");
    this.targetPriv = new File(this.targetProject, "priv");
    this.targetReleases = new File(this.target, "releases");
    this.targetSite = new File(this.target, "site");
    this.targetSrc = new File(this.targetProject, "src");
    this.targetSurefireReports = new File(this.target, "surefire-reports");
    this.targetTest = new File(this.target, this.projectName + "-test");
    this.targetTestEbin = new File(this.targetTest, "ebin");
    this.targetTestPriv = new File(this.targetTest, "priv");
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
  public String projectName() {
    return this.projectName;
  }

  @Override
  public File projectArtifactFile() {
    return new File(this.target, this.projectName + ErlConstants.TARGZ_SUFFIX);
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
  public String cookie() {
    return this.cookie;
  }

  @Override
  public String testNode() {
    return this.testNode;
  }

  @Override
  public File apt() {
    return this.apt;
  }

  @Override
  public File base() {
    return this.base;
  }

  @Override
  public File changes() {
    return this.changes;
  }

  @Override
  public File ebin() {
    return this.ebin;
  }

  @Override
  public File include() {
    return this.include;
  }

  @Override
  public File priv() {
    return this.priv;
  }

  @Override
  public File site() {
    return this.site;
  }

  @Override
  public File src() {
    return this.src;
  }

  @Override
  public File src_base() {
    return this.src_base;
  }

  @Override
  public File test_include() {
    return this.test_include;
  }

  @Override
  public File test_priv() {
    return this.test_priv;
  }

  @Override
  public File test_src() {
    return this.test_src;
  }

  @Override
  public File appFile() {
    return new File(this.ebin, this.project.getArtifactId() + ErlConstants.APP_SUFFIX);
  }

  @Override
  public File appupFile() {
    return new File(this.ebin, this.project.getArtifactId() + ErlConstants.APPUP_SUFFIX);
  }

  @Override
  public File relFile() {
    return new File(this.base, this.project.getArtifactId() + ErlConstants.REL_SUFFIX);
  }

  @Override
  public File relupFile() {
    return new File(this.base, ErlConstants.RELUP);
  }

  @Override
  public File sysConfigFile() {
    return new File(this.base, ErlConstants.SYS_CONFIG);
  }

  @Override
  public File target() {
    return this.target;
  }

  @Override
  public File targetEbin() {
    return this.targetEbin;
  }

  @Override
  public File targetInclude() {
    return this.targetInclude;
  }

  @Override
  public File targetLib() {
    return this.targetLib;
  }

  @Override
  public File targetMibs() {
    return this.targetMibs;
  }

  @Override
  public File targetPriv() {
    return this.targetPriv;
  }

  @Override
  public File targetProject() {
    return this.targetProject;
  }

  @Override
  public File targetReleases() {
    return this.targetReleases;
  }

  @Override
  public File targetSite() {
    return this.targetSite;
  }

  @Override
  public File targetSrc() {
    return this.targetSrc;
  }

  @Override
  public File targetSurefireReports() {
    return this.targetSurefireReports;
  }

  @Override
  public File targetTest() {
    return this.targetTest;
  }

  @Override
  public File targetTestEbin() {
    return this.targetTestEbin;
  }

  @Override
  public File targetTestPriv() {
    return this.targetTestPriv;
  }

  @Override
  public File targetAppFile() {
    return new File(this.targetEbin, this.project.getArtifactId() + ErlConstants.APP_SUFFIX);
  }

  @Override
  public File targetAppupFile() {
    return new File(this.targetEbin, this.project.getArtifactId() + ErlConstants.APPUP_SUFFIX);
  }

  @Override
  public File targetRelFile() {
    return new File(this.target, this.projectName + ErlConstants.REL_SUFFIX);
  }

  @Override
  public File targetRelupFile() {
    return new File(this.target, ErlConstants.RELUP);
  }

  @Override
  public File targetSysConfigFile() {
    return new File(this.target, ErlConstants.SYS_CONFIG);
  }
}