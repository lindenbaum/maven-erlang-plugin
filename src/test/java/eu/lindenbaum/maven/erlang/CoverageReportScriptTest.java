package eu.lindenbaum.maven.erlang;

import static org.easymock.EasyMock.createStrictControl;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import eu.lindenbaum.maven.erlang.CoverageReportResult.Report;
import eu.lindenbaum.maven.erlang.CoverageReportResult.Report.Module;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.easymock.IMocksControl;
import org.junit.Before;
import org.junit.Test;

public class CoverageReportScriptTest {
  private IMocksControl control;
  private Log log;

  @Before
  public void setUp() {
    this.control = createStrictControl();
    this.log = this.control.createMock("log", Log.class);
  }

  @Test
  public void testGet() throws MojoExecutionException {
    File testDir = new File("testDir");
    List<File> tests = Arrays.asList(new File("test1"), new File("test2"));
    List<File> sources = Arrays.asList(new File("source1"), new File("source2"));
    CoverageReportScript script = new CoverageReportScript(testDir, tests, sources);
    String expression = script.get();
    assertNotNull(expression);
    assertFalse(expression.isEmpty());
    assertFalse(expression.contains("%s"));
  }

  @Test
  public void testCoverageWithModules() throws MojoExecutionException {
    this.control.replay();

    List<OtpErlangObject> rows = new ArrayList<OtpErlangObject>();

    rows.add(makeModuleRow("mod_1", 1L, 2L));
    rows.add(makeModuleRow("mod_2", 2L, 1L));
    rows.add(makeModuleRow("mod_3", 3L, 0L));

    rows.add(makeFunctionRow("mod_1", "fun_1", 0, 1L, 0L));
    rows.add(makeFunctionRow("mod_1", "fun_2", 0, 0L, 1L));
    rows.add(makeFunctionRow("mod_1", "fun_3", 0, 0L, 1L));

    rows.add(makeFunctionRow("mod_2", "fun_1", 0, 1L, 0L));
    rows.add(makeFunctionRow("mod_2", "fun_2", 0, 1L, 0L));
    rows.add(makeFunctionRow("mod_2", "fun_3", 0, 0L, 1L));

    rows.add(makeFunctionRow("mod_3", "fun_1", 0, 1L, 0L));
    rows.add(makeFunctionRow("mod_3", "fun_2", 0, 1L, 0L));
    rows.add(makeFunctionRow("mod_3", "fun_3", 0, 1L, 0L));

    rows.add(makeClauseRow("mod_1", "fun_1", 0, 0, 1L, 0L));
    rows.add(makeClauseRow("mod_1", "fun_2", 0, 0, 0L, 1L));
    rows.add(makeClauseRow("mod_1", "fun_3", 0, 0, 0L, 1L));

    rows.add(makeClauseRow("mod_2", "fun_1", 0, 0, 1L, 0L));
    rows.add(makeClauseRow("mod_2", "fun_2", 0, 0, 1L, 0L));
    rows.add(makeClauseRow("mod_2", "fun_3", 0, 0, 0L, 1L));

    rows.add(makeClauseRow("mod_3", "fun_1", 0, 0, 1L, 0L));
    rows.add(makeClauseRow("mod_3", "fun_2", 0, 0, 1L, 0L));
    rows.add(makeClauseRow("mod_3", "fun_3", 0, 0, 1L, 0L));

    rows.add(makeLineRow("mod_1", 6, 1L, 0L));
    rows.add(makeLineRow("mod_1", 9, 0L, 1L));
    rows.add(makeLineRow("mod_1", 12, 0L, 1L));

    rows.add(makeLineRow("mod_2", 6, 1L, 0L));
    rows.add(makeLineRow("mod_2", 9, 1L, 0L));
    rows.add(makeLineRow("mod_2", 12, 0L, 1L));

    rows.add(makeLineRow("mod_3", 6, 1L, 0L));
    rows.add(makeLineRow("mod_3", 9, 1L, 0L));
    rows.add(makeLineRow("mod_3", 12, 1L, 0L));

    OtpErlangList list = new OtpErlangList(rows.toArray(new OtpErlangObject[0]));
    OtpErlangAtom level = new OtpErlangAtom("ok");
    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ level, list });

    File testDir = new File("testDir");
    List<File> tests = Arrays.asList(new File("test1"), new File("test2"));
    List<File> sources = Arrays.asList(new File("source1"), new File("source2"));

    CoverageReportScript script = new CoverageReportScript(testDir, tests, sources);
    CoverageReportResult coverageResult = script.handle(result);

    assertFalse(coverageResult.failed());
    coverageResult.logOutput(this.log);

    Report data = coverageResult.getReport();
    assertNotNull(data);

    assertEquals(66, data.getCoverage());
    assertEquals(3, data.getNumberOfModules());
    assertEquals(9, data.getNumberOfFunctions());
    assertEquals(9, data.getNumberOfClauses());
    assertEquals(9, data.getNumberOfLines());
    assertEquals(6, data.getNumberOfCoveredLines());
    assertEquals(3, data.getNumberOfNotCoveredLines());

    Module module1 = (Module) data.getModules().toArray()[0];
    Module module2 = (Module) data.getModules().toArray()[1];
    Module module3 = (Module) data.getModules().toArray()[2];

    assertEquals(3, module1.getNumberOfFunctions());
    assertEquals(3, module2.getNumberOfFunctions());
    assertEquals(3, module3.getNumberOfFunctions());

    assertEquals(3, module1.getNumberOfClauses());
    assertEquals(3, module2.getNumberOfClauses());
    assertEquals(3, module3.getNumberOfClauses());

    assertEquals(3, module1.getNumberOfLines());
    assertEquals(3, module2.getNumberOfLines());
    assertEquals(3, module3.getNumberOfLines());

    assertEquals(33, module1.getCoverage());
    assertEquals(66, module2.getCoverage());
    assertEquals(100, module3.getCoverage());

    assertTrue(module1.isCovered(6));
    assertFalse(module1.isCovered(9));
    assertFalse(module1.isCovered(12));

    assertTrue(module2.isCovered(6));
    assertTrue(module2.isCovered(9));
    assertFalse(module2.isCovered(12));

    assertTrue(module3.isCovered(6));
    assertTrue(module3.isCovered(9));
    assertTrue(module3.isCovered(12));

    this.control.verify();
  }

  private OtpErlangTuple makeModuleRow(String name, long covered, long notCovered) {
    OtpErlangAtom _type = new OtpErlangAtom("module");
    OtpErlangAtom _module = new OtpErlangAtom(name);
    OtpErlangLong _covered = new OtpErlangLong(covered);
    OtpErlangLong _notCovered = new OtpErlangLong(notCovered);
    return new OtpErlangTuple(new OtpErlangObject[]{ _type, _module, _covered, _notCovered });
  }

  private OtpErlangTuple makeFunctionRow(String module, String name, int arity, long covered, long notCovered) {
    OtpErlangAtom _type = new OtpErlangAtom("function");
    OtpErlangAtom _module = new OtpErlangAtom(module);
    OtpErlangAtom _name = new OtpErlangAtom(name);
    OtpErlangInt _arity = new OtpErlangInt(arity);
    OtpErlangLong _covered = new OtpErlangLong(covered);
    OtpErlangLong _notCovered = new OtpErlangLong(notCovered);
    return new OtpErlangTuple(new OtpErlangObject[]{ _type, _module, _name, _arity, _covered, _notCovered });
  }

  private OtpErlangObject makeClauseRow(String module,
                                        String function,
                                        int arity,
                                        int index,
                                        long covered,
                                        long notCovered) {
    OtpErlangAtom _type = new OtpErlangAtom("clause");
    OtpErlangAtom _module = new OtpErlangAtom(module);
    OtpErlangAtom _function = new OtpErlangAtom(function);
    OtpErlangInt _arity = new OtpErlangInt(arity);
    OtpErlangInt _index = new OtpErlangInt(index);
    OtpErlangLong _covered = new OtpErlangLong(covered);
    OtpErlangLong _notCovered = new OtpErlangLong(notCovered);
    return new OtpErlangTuple(new OtpErlangObject[]{ _type, _module, _function, _arity, _index, _covered,
                                                    _notCovered });
  }

  private OtpErlangObject makeLineRow(String module, int lineNumber, long covered, long notCovered) {
    OtpErlangAtom _type = new OtpErlangAtom("line");
    OtpErlangAtom _module = new OtpErlangAtom(module);
    OtpErlangInt _lineNumber = new OtpErlangInt(lineNumber);
    OtpErlangLong _covered = new OtpErlangLong(covered);
    OtpErlangLong _notCovered = new OtpErlangLong(notCovered);
    return new OtpErlangTuple(new OtpErlangObject[]{ _type, _module, _lineNumber, _covered, _notCovered });
  }

  @Test
  public void testCoverageFailed() throws MojoExecutionException {
    this.log.error("");
    this.log.error("message2");
    this.log.error("message3");

    this.control.replay();

    OtpErlangList message1 = new OtpErlangList();
    OtpErlangString message2 = new OtpErlangString("message2");
    OtpErlangString message3 = new OtpErlangString("message3");
    OtpErlangObject[] m = new OtpErlangObject[]{ message1, message2, message3 };
    OtpErlangList messages = new OtpErlangList(m);

    OtpErlangAtom level = new OtpErlangAtom("error");

    OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{ level, messages });

    File testDir = new File("testDir");
    List<File> tests = Arrays.asList(new File("test1"), new File("test2"));
    List<File> sources = Arrays.asList(new File("source1"), new File("source2"));

    CoverageReportScript script = new CoverageReportScript(testDir, tests, sources);
    CoverageReportResult coverageResult = script.handle(result);

    assertTrue(coverageResult.failed());
    coverageResult.logOutput(this.log);

    this.control.verify();
  }
}
