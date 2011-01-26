package eu.lindenbaum.maven.erlang;

import java.io.File;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import eu.lindenbaum.maven.util.ErlUtils;
import eu.lindenbaum.maven.util.MavenUtils;
import eu.lindenbaum.maven.util.MavenUtils.LogLevel;

import org.apache.maven.plugin.logging.Log;

/**
 * A script that returns the coverage report for some project.
 * 
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public class CoverageReportScript implements Script<CoverageReportResult> {
  private static final String script = //
  NL + "Dir = \"%s\"," + NL + //
      "Tests = %s," + NL + //
      "Modules = %s," + NL + //
      "case cover2:compile_beam_directory(Dir, [debug_info, export_all, {d, 'TEST'}]) of " + NL + //
      "   {error, Reason} ->" + NL + //
      "       {error, [lists:flatten(io_lib:format(\"~p\", [Reason]))]};" + NL + //
      "   _ ->" + NL + //
      "       case catch(eunit:test(Tests)) of " + NL + //
      "       ok ->" + NL + //
      "           begin " + NL + //
      "           Levels = [module, function, clause, line]," + NL + //
      "           Results = [{L, cover2:analyse(M, coverage, L)}" + NL + //
      "                  || M <- Modules, L <- Levels]," + NL + //
      "           SortedResults = " + NL + //
      "               lists:foldl(" + NL + //
      "                 fun " + NL + //
      "                 (Mod = {module, _}, {Ms, Fs, Cs, Ls}) ->" + NL + //
      "                          {[Mod | Ms], Fs, Cs, Ls};" + NL + //
      "                 (Fun = {function, _}, {Ms, Fs, Cs, Ls}) ->" + NL + //
      "                          {Ms, [Fun | Fs], Cs, Ls};" + NL + //
      "                 (Clause = {clause, _}, {Ms, Fs, Cs, Ls}) ->" + NL + //
      "                          {Ms, Fs, [Clause | Cs], Ls};" + NL + //
      "                 (Line = {line, _}, {Ms, Fs, Cs, Ls}) ->" + NL + //
      "                          {Ms, Fs, Cs, [Line | Ls]};" + NL + //
      "                 (stop, {Ms, Fs, Cs, Ls}) ->" + NL + //
      "                          Ls ++ Cs ++ Fs ++ Ms" + NL + //
      "                      end," + NL + //
      "                 {[], [], [], []}," + NL + //
      "                 Results ++ [stop])," + NL + //      
      "           Table =" + NL + //
      "               lists:foldl(" + NL + //
      "                 fun " + NL + //
      "                 ({L, {ok, {M, {C, N}}}}, Acc) ->" + NL + //
      "                          [[{L, M, C, N}] | Acc];" + NL + //
      "                 ({L, {ok, Cov}}, Acc) ->" + NL + //
      "                          Rows =" + NL + //
      "                          lists:map(fun({{M, F, A}, {C, N}}) ->" + NL + //
      "                                    {L, M, F, A, C, N};" + NL + //
      "                               ({{M, F, A, I}, {C, N}}) ->" + NL + //
      "                                    {L, M, F, A, I, C, N};" + NL + //
      "                               ({{M, Ln}, {C, N}}) ->" + NL + //
      "                                    {L, M, Ln, C, N}" + NL + //
      "                                end," + NL + //
      "                                Cov)," + NL + //
      "                          [Rows | Acc]" + NL + //
      "                      end," + NL + //
      "                 []," + NL + //
      "                 SortedResults)," + NL + //
      "           {ok, lists:flatten(Table)}" + NL + //
      "           end;" + NL + //
      "       error ->" + NL + //
      "           {error, []};" + NL + //
      "       {Class, Reason} -> " + NL + //
      "           {error, [lists:flatten(io_lib:format(\"~p:~p\", [Class, Reason]))]};" + NL + //
      "       {error, Reason} ->" + NL + //
      "           {error, [lists:flatten(io_lib:format(\"~p\", [Reason]))]}" + NL + //
      "       end " + NL + //
      "end." + NL;

  private final List<File> tests;
  private final File testDir;
  private final List<File> sources;

  public CoverageReportScript(File testDir, List<File> tests, List<File> sources) {
    this.testDir = testDir;
    this.tests = tests;
    this.sources = sources;
  }

  @Override
  public String get() {
    String testPath = this.testDir.getAbsolutePath();
    String testModuleList = ErlUtils.toModuleList(this.tests, "'", "'");
    String sourceModuleList = ErlUtils.toModuleList(this.sources, "'", "'");
    return String.format(script, testPath, testModuleList, sourceModuleList);
  }

  @Override
  public CoverageReportResult handle(OtpErlangObject output) {
    OtpErlangTuple resultTuple = (OtpErlangTuple) output;
    final String result = ErlUtils.toString(resultTuple.elementAt(0));
    final OtpErlangList resultList = (OtpErlangList) resultTuple.elementAt(1);
    return new CoverageReportResult() {

      @Override
      public boolean failed() {
        return "error".equals(result);
      }

      @Override
      public void logOutput(Log log) {
        if (failed()) {
          for (int i = 0; i < resultList.arity(); ++i) {
            String message = ErlUtils.toString(resultList.elementAt(i));
            MavenUtils.logMultiLineString(log, LogLevel.ERROR, message);
          }
        }
      }

      @Override
      public Report getReport() {
        return new Report(resultList);
      }
    };
  }
}
