import java.security.*;
import java.io.*;

File tempDir = new File(basedir, "target/package-0");
if (!tempDir.isDirectory()) {
    throw new IllegalStateException("Target package directory " + tempDir + " was missing.");
}

File target = new File(basedir, "target/package-0.tar.gz");
if (!target.isFile()) {
    throw new IllegalStateException("Target package file " + target + " was missing.");
}

File actual = new File(basedir, "target/package-0/ebin/test_server.beam");
File expected = new File(basedir, "target/ebin/test_server.beam");
MessageDigest md = MessageDigest.getInstance("MD5");
md.reset();
InputStream is1 = new FileInputStream(expected);
try {
  is1 = new DigestInputStream(is1, md);
  while (is1.available()) {
    is1.read();
  }
}
finally {
  is1.close();
}
byte[] digestExpected = md.digest();

md.reset();
InputStream is2 = new FileInputStream(actual);
try {
  is2 = new DigestInputStream(is2, md);
  while (is2.available()) {
    is2.read();
  }
}
finally {
  is2.close();
}
byte[] digestActual = md.digest();

if (digestActual.length != digestActual.length) {
  throw new IllegalStateException("Beam file checksum length differs, package seems corrupt.");
}

for (int i = 0; i < digestExpected.length; i++) {
  if (digestActual[i] != digestExpected[i]) {
    print "Checksum expected: " + digestExpected + "\n";
    print "Checksum actual:   " + digestActual + "\n";
    throw new IllegalStateException("Beam file checksum differs from expected, package seems corrupt.");
  }
} 

return true;
