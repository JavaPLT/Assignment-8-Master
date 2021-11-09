import junit.framework.TestCase;
import java.io.*;

/** A JUnit test case class. This test assumes that the filed littleData1, littleData2 are stored in a sibling
  * direction named "Data".  The "sibling" relationship is with the folder containing the simplifer (including the
  * Parser class. */
public class BoolSimpFileTest extends TestCase {
  /* test methods */
  public void testSmallDNF() throws IOException {
    Parser p = new Parser("(| (& x y) (| (& x (! y)) (| (& (! x) y) (& (! x) (! y)))))");
    String result = p.reduce();
    assertEquals("testSmallDNF", "T", result);
  }
 
  public void testLittleData1() throws IOException {
    Parser p = new Parser(new BufferedReader(new FileReader("../Data/littleData1")));
    String result = p.reduce();
    assertEquals("littleData1", "T", result);
  }
  
  public void testLittleData2() throws IOException {
  Parser p = new Parser(new BufferedReader(new FileReader("../Data/littleData2")));
    String result = p.reduce();
    assertEquals("littleData2", "T", result);
  }
  
  public void testLittleData3() throws IOException {
  Parser p = new Parser(new BufferedReader(new FileReader("../Data/littleData3")));
    String result = p.reduce();
    assertEquals("littleData3", "(> h (> g (> f (> e (> d (> c (! b)))))))", result);
  }
  
  public void testLittleData4() throws IOException {
    Parser p = new Parser(new BufferedReader(new FileReader("../Data/littleData4")));
    String result = p.reduce();
    assertEquals("littleData4", "(> h (> g (> f (> e (| d (| c (| b a)))))))", result);
  }
}
