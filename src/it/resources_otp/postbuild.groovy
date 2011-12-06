File edoc = new File(basedir, "target/overview.edoc");
if (!edoc.isFile()) {
    throw new IllegalStateException("Missing resource, the file " + edoc + " should have been generated.");
}

String doc = edoc.getText();

if (doc.trim() == "") {
  throw new IllegalStateException("Resource document must not be empty.");
}

["Replaced: ARTIFACT with \${ARTIFACT}",
 "Replaced: DESCRIPTION with \${DESCRIPTION}",
 "Replaced: ID with \${ID}",
 "Replaced: VERSION with \${VERSION}"].each { entry ->
  if (doc.indexOf(entry) > -1) {
    throw new IllegalStateException("Converted line " + entry + " must not be present!");
  }
}

return true;
