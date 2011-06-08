package eu.lindenbaum.maven.mojo.app;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class ProfilerTest {

  @Test
  public void testPlainTextFormatting() {
    String formatting = "%1$-68s %2$6d %3$6d %4$9.2f";
    assertEquals("erlang:setelement/3                                                   80008  19875      0.25",
                 String.format(formatting, "erlang:setelement/3", 80008, 19875, 0.25f));
    assertEquals("erlang:whereis/1                                                       3997  17084      4.27",
                 String.format(formatting, "erlang:whereis/1", 3997, 17084, 4.27f));
    assertEquals("erlang:load_module/2                                                    112  12788    114.18",
                 String.format(formatting, "erlang:load_module/2", 112, 12788, 114.18f));
  }
}
