// Java dialect: Java 5.0
import java.io.*;


/* Boolean Simplifier

   This program reads a stream of formulas expressed in parenthesized
   prefix syntax and outputs a corresponding stream of simplified formulas.
   All tautologies are simplied to "T" and all contradictions to "F".

/** The Parser class for the boolean simplifier.  The parse routine is a method in Parser called read(); it returns
 * a an instance of the Form abstract syntax type (an interface). */ 
class Parser extends StreamTokenizer {
  
  // A Parser is a file containing a textual (ASCII) representation of a Form object.
  
  // short names for StreamTokenizer codes
  
  public static final int WORD = StreamTokenizer.TT_WORD; 
  public static final int EOF = StreamTokenizer.TT_EOF; 
  public static final int EOL = StreamTokenizer.TT_EOL; 
  
  Parser(File file) throws IOException { this(new BufferedReader(new FileReader(file))); }
  
  Parser(Reader r) {
         
    super(r);
    
    // configure StreamTokenizer portion of this
    super.resetSyntax();   // the super prefix is forced by a LL type checking bug
    wordChars('0','9');
    wordChars('a','z');
    wordChars('A','Z');
    whitespaceChars(0,' '); 
  }
 
  Parser(String text) { super(new StringReader(text)); }
  
  /** Parses the formula expressed in "abbreviated Scheme syntax" in the reader r (a String or a File).  It throws
    * a ParseException if it encounters a syntax error.  */
  public Form read() throws IOException {
    
    int token = nextToken();
    
    if (token == WORD) { 
      if (sval.equals("T")) return Constant.TRUE;
      else if (sval.equals("F")) return Constant.FALSE;
      else return Variable.makeVariable(sval);
    }
    else if (token == '(') {
      token = nextToken();
      if (token == '!') {
        Form arg = read();
        token = nextToken(); // read trailing parenthesis
        if (token != ')') 
          throw new ParseException("wrong number of arguments to !");
        return new Not(arg);
      }
      else if (token == '&') {
        Form arg1 = read();
        Form arg2 = read();
        token = nextToken(); // read trailing parenthesis
        if (token != ')') 
          throw new ParseException("wrong number of arguments to &");
        return new And(arg1,arg2);
      }
      else if (token == '|') {
        Form arg1 = read();
        Form arg2 = read();
        token = nextToken(); // read trailing parenthesis
        if (token != ')') 
          throw new ParseException("wrong number of arguments to |");
        return new Or(arg1,arg2);
      }
      else if (token == '>') {
        Form arg1 = read();
        Form arg2 = read();
        token = nextToken(); // read trailing parenthesis
        if (token != ')') 
          throw new ParseException("wrong number of arguments to >");
        return new Implies(arg1,arg2);
      }
      else if (token == '?') {
        Form arg1 = read();
        Form arg2 = read();
        Form arg3 = read();
        token = nextToken(); // read trailing parenthesis
        if (token != ')') 
          throw new ParseException("wrong number of arguments to ?");
        return new If(arg1,arg2,arg3);
      }
      else throw new ParseException("operator " + toString() + " not recognized");
    }
    else if (token == EOF) return null;
    else if (token == ')') throw new ParseException("unbalanced ')'");
    else throw new ParseException("operator " + toString() + " not recognized");
  }
  
  public String reduce() throws IOException {
    Form g = read();
    IfForm h = g.accept(ConvertToIf.ONLY);
    IfForm i = h.accept(Normalize.ONLY);
    IfForm j = i.accept(Evaluate.BASE);
    Form k = j.accept(ConvertToBool.ONLY);
    return k.accept(Print.ONLY);
  } 
}

/** Exception class for Parser syntax errors. */
class ParseException extends IOException {
  ParseException(String s) { super(s); }
}

