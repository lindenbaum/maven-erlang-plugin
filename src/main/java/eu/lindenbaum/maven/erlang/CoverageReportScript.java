package eu.lindenbaum.maven.erlang;

import java.io.File;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import eu.lindenbaum.maven.util.ErlUtils;

import org.apache.maven.plugin.logging.Log;

/**
 * A script that returns the coverage report for some project.
 * 
 * @author Olle Törnström <olle.toernstroem@lindenbaum.eu>
 */
public class CoverageReportScript implements Script<CoverageReportResult> {
  private static final String script = //
  "    Dir = \"%s\"," + //
      "Tests = %s," + //
      "Modules = %s," + //
      "case cover2:compile_beam_directory(Dir) of " + //
      "   {error, Reason} ->" + //
      "       {error, [lists:flatten(io_lib:format(\"~p\", [Reason]))]};" + //
      "   _ ->" + //
      "       case catch(eunit:test(Tests)) of " + //
      "       ok ->" + //
      "           begin " + //
      "           Levels = [module, function, clause, line]," + //
      "           Results = [{L, cover2:analyse(M, coverage, L)}" + //
      "                  || M <- Modules, L <- Levels]," + //
      "           SortedResults = " + //
      "               lists:foldl(" + //
      "                 fun " + //
      "                 (Mod = {module, _}, {Ms, Fs, Cs, Ls}) ->" + //
      "                          {[Mod | Ms], Fs, Cs, Ls};" + //
      "                 (Fun = {function, _}, {Ms, Fs, Cs, Ls}) ->" + //
      "                          {Ms, [Fun | Fs], Cs, Ls};" + //
      "                 (Clause = {clause, _}, {Ms, Fs, Cs, Ls}) ->" + //
      "                          {Ms, Fs, [Clause | Cs], Ls};" + //
      "                 (Line = {line, _}, {Ms, Fs, Cs, Ls}) ->" + //
      "                          {Ms, Fs, Cs, [Line | Ls]};" + //
      "                 (stop, {Ms, Fs, Cs, Ls}) ->" + //
      "                          Ls ++ Cs ++ Fs ++ Ms" + //
      "                      end," + //
      "                 {[], [], [], []}," + //
      "                 Results ++ [stop])," + //      
      "           Table =" + //
      "               lists:foldl(" + //
      "                 fun " + //
      "                 ({L, {ok, {M, {C, N}}}}, Acc) ->" + //
      "                          [[{L, M, C, N}] | Acc];" + //
      "                 ({L, {ok, Cov}}, Acc) ->" + //
      "                          Rows =" + //
      "                          lists:map(fun({{M, F, A}, {C, N}}) ->" + //
      "                                    {L, M, F, A, C, N};" + //
      "                               ({{M, F, A, I}, {C, N}}) ->" + //
      "                                    {L, M, F, A, I, C, N};" + //
      "                               ({{M, Ln}, {C, N}}) ->" + //
      "                                    {L, M, Ln, C, N}" + //
      "                                end," + //
      "                                Cov)," + //
      "                          [Rows | Acc]" + //
      "                      end," + //
      "                 []," + //
      "                 SortedResults)," + //
      "           {ok, lists:flatten(Table)}" + //
      "           end;" + //
      "       error ->" + //
      "           {error, []};" + //
      "       {Class, Reason} -> " + //
      "           {error, [lists:flatten(io_lib:format(\"~p:~p\", [Class, Reason]))]};" + //
      "       {error, Reason} ->" + //
      "           {error, [lists:flatten(io_lib:format(\"~p\", [Reason]))]}" + //
      "       end " + //
      "end.";

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
    final OtpErlangAtom result = (OtpErlangAtom) resultTuple.elementAt(0);
    final OtpErlangList resultList = (OtpErlangList) resultTuple.elementAt(1);
    return new CoverageReportResult() {

      @Override
      public boolean failed() {
        return "error".equals(ErlUtils.cast(result));
      }

      @Override
      public void logOutput(Log log) {
        if (failed()) {
          for (int i = 0; i < resultList.arity(); ++i) {
            String multiLine = ErlUtils.cast(resultList.elementAt(i));
            String[] lines = multiLine.split("\r?\n");
            for (String line : lines) {
              log.error(line);
            }
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
