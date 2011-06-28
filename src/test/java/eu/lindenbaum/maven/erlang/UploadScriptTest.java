package eu.lindenbaum.maven.erlang;

import static org.easymock.EasyMock.createStrictControl;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.easymock.IMocksControl;
import org.junit.Before;
import org.junit.Test;

public class UploadScriptTest {
  private IMocksControl control;
  private Log log;

  @Before
  public void setUp() {
    this.control = createStrictControl();
    this.log = this.control.createMock("log", Log.class);
  }

  @Test
  public void testGet() throws MojoExecutionException {
    String remoteNode = "node@otherhost.de";
    List<File> modules = Arrays.asList(new File("module"));
    List<File> appFiles = Arrays.asList(new File("appFile"));
    List<File> resources = Arrays.asList(new File("resourceFile"));
    UploadScript script = new UploadScript(remoteNode, modules, appFiles, resources);
    String expression = script.get();
    assertNotNull(expression);
    assertFalse(expression.isEmpty());
    assertFalse(expression.contains("%s"));
  }

  @Test
  public void testHandleOk() throws MojoExecutionException {
    this.log.info("Files uploaded successfully to node@otherhost.de:");
    this.log.info(" * module");
    this.log.info("");

    this.control.replay();

    OtpErlangString module = new OtpErlangString("module");
    OtpErlangList succeeded = new OtpErlangList(new OtpErlangObject[]{ module });
    OtpErlangList failed = new OtpErlangList();
    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ succeeded, failed });

    String remoteNode = "node@otherhost.de";
    List<File> modules = Arrays.asList(new File("module"));
    List<File> appFiles = Arrays.asList(new File("appFile"));
    List<File> resources = Arrays.asList(new File("resourceFile"));
    UploadScript script = new UploadScript(remoteNode, modules, appFiles, resources);
    GenericScriptResult genericScriptResult = script.handle(result);
    assertTrue(genericScriptResult.success());
    genericScriptResult.logOutput(this.log);

    this.control.verify();
  }

  @Test
  public void testHandleError() throws MojoExecutionException {
    this.log.info("Files uploaded successfully to node@otherhost.de:");
    this.log.info(" * module");
    this.log.info("");
    this.log.error("Files that could not be uploaded to node@otherhost.de:");
    this.log.error(" * resource");

    this.control.replay();

    OtpErlangString module = new OtpErlangString("module");
    OtpErlangString resource = new OtpErlangString("resource");
    OtpErlangList succeeded = new OtpErlangList(new OtpErlangObject[]{ module });
    OtpErlangList failed = new OtpErlangList(new OtpErlangObject[]{ resource });
    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ succeeded, failed });

    String remoteNode = "node@otherhost.de";
    List<File> modules = Arrays.asList(new File("module"));
    List<File> appFiles = Arrays.asList(new File("appFile"));
    List<File> resources = Arrays.asList(new File("resourceFile"));
    UploadScript script = new UploadScript(remoteNode, modules, appFiles, resources);
    GenericScriptResult genericScriptResult = script.handle(result);
    assertFalse(genericScriptResult.success());
    genericScriptResult.logOutput(this.log);

    this.control.verify();
  }
}
