package eu.lindenbaum.maven.mojo.rel;

import static org.easymock.EasyMock.createControl;

import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.logging.Log;
import org.easymock.IMocksControl;
import org.junit.Before;
import org.junit.Test;

public class TestRunnerTest {

  private IMocksControl control;
  private Log log;

  @Before
  public void setUp() {
    this.control = createControl();
    this.log = this.control.createMock("log", Log.class);
  }

  @Test
  public void allMinorReleasesForSingleMajorWithWildcardAreValid() throws MojoFailureException {
    TestRunner.checkOtpReleaseVersion(this.log, "R14B*", "R14B01");
    TestRunner.checkOtpReleaseVersion(this.log, "R14B*", "R14B02");
    TestRunner.checkOtpReleaseVersion(this.log, "R14B*", "R14B03");
    TestRunner.checkOtpReleaseVersion(this.log, "R14B*", "R14B04");
  }

  @Test(expected = MojoFailureException.class)
  public void lowerMajorVersionCausesErrorToBeThrown() throws MojoFailureException {
    TestRunner.checkOtpReleaseVersion(this.log, "R14B*", "R12B");
  }

  @Test
  public void exactMatchingVersionsAreValid() throws MojoFailureException {
    TestRunner.checkOtpReleaseVersion(this.log, "R15B", "R15B");
  }

  @Test
  public void twoOptionalMajorVersionsAreBothValid() throws MojoFailureException {
    TestRunner.checkOtpReleaseVersion(this.log, "R14B*|R15B*", "R14B");
    TestRunner.checkOtpReleaseVersion(this.log, "R14B*|R15B*", "R14B01");
    TestRunner.checkOtpReleaseVersion(this.log, "R14B*|R15B*", "R14B02");
    TestRunner.checkOtpReleaseVersion(this.log, "R14B*|R15B*", "R14B03");
    TestRunner.checkOtpReleaseVersion(this.log, "R14B*|R15B*", "R14B04");
    TestRunner.checkOtpReleaseVersion(this.log, "R14B*|R15B*", "R15B");
    TestRunner.checkOtpReleaseVersion(this.log, "R14B*|R15B*", "R15B01");
    TestRunner.checkOtpReleaseVersion(this.log, "R14B*|R15B*", "R15B02");
    TestRunner.checkOtpReleaseVersion(this.log, "R14B*|R15B*", "R15B03");
    TestRunner.checkOtpReleaseVersion(this.log, "R14B*|R15B*", "R15B04");
  }

  @Test(expected = MojoFailureException.class)
  public void noneOfTwoOptionalMajorVersionsAreValid() throws MojoFailureException {
    TestRunner.checkOtpReleaseVersion(this.log, "R14B*|R15B*", "R13B03");
  }

}
