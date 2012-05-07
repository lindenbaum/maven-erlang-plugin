File lexer = new File(basedir, "target/leex_std-0/ebin/lexer.beam");
if (!lexer.isFile()) {
    throw new IllegalStateException("Lexer " + lexer + " is missing.");
}
