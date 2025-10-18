/*
 * Copyright 2009 Yuichiro Moriguchi
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.morilib.lisp.test;

import java.io.IOException;
import java.math.BigInteger;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispComplex;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Parser;
import net.morilib.lisp.ReadException;

public class ParserTest extends TCLisp {

	private static Parser getParser() {
		return new Parser();
	}

	private static Datum read(Parser p, String exp) throws IOException {
		p.clear();
		p.read(exp);
		return p.parse() ? p.getDatum() : null;
	}

	private static void eq(Parser p, String exp, Datum d
			) throws IOException {
		eq(read(p, exp), d);
	}

	private static void eqv(Parser p, String exp, Datum d
			) throws IOException {
		eqv(read(p, exp), d);
	}

	private static void equal(
			Parser p, String exp, Datum d) throws IOException {
		equal(read(p, exp), d);
	}

	private static void eqstr(
			Parser p, String exp, String s) throws IOException {
		eqstr(read(p, exp), str(s));
	}

	private static void eqsym(Parser p, String s) throws IOException {
		eq(p, s, sym(s));
	}

	private static void eqrat(
			Parser p, String s, String n, String d) throws IOException {
		eqv(p, s, newQ(new BigInteger(n), new BigInteger(d)));
	}

	private static void eqrat(
			Parser p, String s, int n, int d) throws IOException {
		eqv(p, s, newQ(n, d));
	}

	private static void eqbig(
			Parser p, String v) throws IOException {
		eqv(p, v, newZ(new BigInteger(v.replaceFirst("^\\+", ""))));
	}

	private static void eqint(
			Parser p, String v, int n) throws IOException {
		eqv(p, v, newZ(n));
	}

	private static void eqdbl(
			Parser p, String v, double d) throws IOException {
		eqv(p, v, newR(d));
	}

	private static void eqimg(
			Parser p, String v, double d) throws IOException {
		eqv(p, v, newC(0, d));
	}

	private static void eqimgE(
			Parser p, String v, int d) throws IOException {
		eqv(p, v, LispComplex.newComplex(newZ(0), newZ(d)));
	}

	private static void eqimgE(
			Parser p, String v, LispReal d) throws IOException {
		eqv(p, v, LispComplex.newComplex(newZ(0), d));
	}

	private static void eqpol(
			Parser p, String v, double r, double t) throws IOException {
//		eqv(p, v, newC(r * Math.cos(t), r * Math.sin(t)));
		eqv(p, v, LispComplex.newPolar(r, t));
	}

	private static void eqcmp(
			Parser p, String v, double r, double i) throws IOException {
		eqv(p, v, newC(r, i));
	}

	private static void eqcmpE(
			Parser p, String v, int r, int i) throws IOException {
		eqv(p, v, LispComplex.newComplex(newZ(r), newZ(i)));
	}

	private static void eqcmpE(
			Parser p, String v, LispReal r, LispReal i) throws IOException {
		eqv(p, v, LispComplex.newComplex(r, i));
	}

	private static void rderr(Parser p, String v) throws IOException {
		try {
			eq(p, v, F);
			fail();
		} catch(ReadException e) {}
	}


	public void testSymbol() throws Exception {
		Parser p = getParser();

		eqsym(p, "symbol");
		eqsym(p, "*");
		eqsym(p, "+");       eqsym(p, "-");
		eqsym(p, "i");
		eqsym(p, "1a");
		eqsym(p, "+1a");     eqsym(p, "-1a");
		eqsym(p, "+-1");
		eqsym(p, ".sym");
		eqsym(p, "+.a");
		eqsym(p, "1.a");
		eqsym(p, "+1.a");    eqsym(p, "-1.a");
		eqsym(p, "1.1a");
		eqsym(p, "1.1e");
		eqsym(p, "1.1e+");   eqsym(p, "1.1e-");
		eqsym(p, "1.1e+a");  eqsym(p, "1.1e-1a");
		eqsym(p, "1.1e+1.1");
		eqsym(p, "/");
		eqsym(p, "1/");
		eqsym(p, "1/2a");
		eqsym(p, "1/2.1");
		eqsym(p, "1.1@");
		eqsym(p, "1.1@+");      eqsym(p, "1.1@-");
		eqsym(p, "1.1@a");      eqsym(p, "1.1@1a");
		eqsym(p, "1.1@.");
		eqsym(p, "1.1@+.");     eqsym(p, "1.1@-.");
		eqsym(p, "1.1@1.");
		eqsym(p, "1.1@1.1e");
		eqsym(p, "1.1@1.1e+");  eqsym(p, "1.1@1.1e-");
		eqsym(p, "1.1@1.1e+1a");
		eqsym(p, "1.1@1.1e+1.1");
		eqsym(p, "1.1@1/");
		eqsym(p, "1.1@1/2a");
		eqsym(p, "1.1@1/2.1");
		eqsym(p, "inf.0");
		eqsym(p, "+in");        eqsym(p, "-in");
		eqsym(p, "+inf");       eqsym(p, "-inf");
		eqsym(p, "+inf.");      eqsym(p, "-inf.");
		eqsym(p, "+inf.00");    eqsym(p, "-inf.00");
		eqsym(p, "+inf.0ii");
		eqsym(p, "nan.0");
		eqsym(p, "+n");
		eqsym(p, "+na");
		eqsym(p, "+nan");
		eqsym(p, "+nan.");
		eqsym(p, "+nan.00");
		eqsym(p, "2i");
		eqsym(p, ".5i");
		eqsym(p, "1e+3i");
		eqsym(p, "1.1ii");
		eqsym(p, "+ii");        eqsym(p, "-ii");
		eqsym(p, "+1.1+1");     eqsym(p, "+1.1-1");
		eqsym(p, "+1.1+ai");    eqsym(p, "+1.1-ai");
		eqsym(p, "+1.1+.i");    eqsym(p, "+1.1-.i");
		eqsym(p, "+1.1+1.1");   eqsym(p, "+1.1-1.1");
		eqsym(p, "+1.1+1.ai");  eqsym(p, "+1.1-1.ai");
		eqsym(p, "1+1e");       eqsym(p, "1-1e");
		eqsym(p, "1+1ei");      eqsym(p, "1-1ei");
		eqsym(p, "1+1e+");      eqsym(p, "1-1e+");
		eqsym(p, "1+1e+i");     eqsym(p, "1-1e+i");
		eqsym(p, "1+1e-i");     eqsym(p, "1-1e-i");
		eqsym(p, "1+1e1");      eqsym(p, "1-1e1");
		eqsym(p, "1+1e+1");     eqsym(p, "1-1e+1");
		eqsym(p, "1+1e-1");     eqsym(p, "1-1e-1");
		eqsym(p, "1+1e+ai");    eqsym(p, "1-1e+ai");
		eqsym(p, "1+1e-ai");    eqsym(p, "1-1e-ai");
		eqsym(p, "1+/");        eqsym(p, "1-/");
		eqsym(p, "1+/i");       eqsym(p, "1-/i");
		eqsym(p, "1+1/");       eqsym(p, "1-1/");
		eqsym(p, "1+1/i");      eqsym(p, "1-1/i");
		eqsym(p, "1+1/a");      eqsym(p, "1-1/a");
		eqsym(p, "1+1/ai");     eqsym(p, "1-1/ai");
		eqsym(p, "1+in");       eqsym(p, "1-in");
		eqsym(p, "1+inf");      eqsym(p, "1-inf");
		eqsym(p, "1+inf.");     eqsym(p, "1-inf.");
		eqsym(p, "1+inf.0");    eqsym(p, "1-inf.0");
		eqsym(p, "1+inf.00i");  eqsym(p, "1-inf.00i");
		eqsym(p, "1+ii");       eqsym(p, "1-ii");
		eqsym(p, "1+1ii");      eqsym(p, "1-1ii");
	}

	public void testInteger() throws Exception {
		Parser p = getParser();

		eqbig(p, "999999999999999999999999999999999999999999999999999");
		eqbig(p, "+10000000");
		eqbig(p, "-10000000");
	}

	public void testRational() throws Exception {
		Parser p = getParser();
		final String N = "1111111111111111111111111111111111111111111";
		final String D = "1234567890123456789012345678901234567890123";

		eqrat(p, N + "/" + D, N, D);
		eqv(p, "10/10", newZ(1));
		eqv(p, "+13/11", newQ(13, 11));
		eqv(p, "-13/11", newQ(-13, 11));
	}

	public void testDouble() throws Exception {
		Parser p = getParser();

		eqdbl(p, ".5", 0.5);
		eqdbl(p, "+.5", 0.5);     eqdbl(p, "-.5", -0.5);
		eqdbl(p, "1.5", 1.5);
		eqdbl(p, "+1.5", 1.5);    eqdbl(p, "-1.5", -1.5);
		eqdbl(p, "1e3", 1e3);
		eqdbl(p, "1e+3", 1e3);    eqdbl(p, "1e-3", 1e-3);
	}

	public void testImaginary() throws Exception {
		Parser p = getParser();

		eqimgE(p, "+i", 1);       eqimgE(p, "-i", -1);
		eqimg(p, "+.5i", 0.5);    eqimg(p, "-.5i", -0.5);
		eqimg(p, "+1.5i", 1.5);   eqimg(p, "-1.5i", -1.5);
		eqimg(p, "+1e+3i", 1e3);  eqimg(p, "-1e-3i", -1e-3);

		eqimgE(p, "+1/2i", newQ(1, 2));
		eqimgE(p, "-1/2i", newQ(-1, 2));
		eqimgE(p, "+3/3i", newZ(1));
	}

	public void testPolar() throws Exception {
		Parser p = getParser();

		eqpol(p, "1.0@0.7", 1.0, 0.7);
		eqpol(p, "1.0@+0.7", 1.0, 0.7);
		eqpol(p, "1.0@-0.7", 1.0, -0.7);
		eqpol(p, "1.0@.7", 1.0, 0.7);
		eqpol(p, "1.0@+.7", 1.0, 0.7);
		eqpol(p, "1.0@-.7", 1.0, -0.7);
		eqpol(p, "1.0@1.0", 1.0, 1.0);
		eqpol(p, "1.0@+1.0", 1.0, 1.0);
		eqpol(p, "1.0@-1.0", 1.0, -1.0);
		eqpol(p, "1.0@1e1", 1.0, 1e1);
		eqpol(p, "1.0@1e+1", 1.0, 1e1);
		eqpol(p, "1.0@1e-1", 1.0, 1e-1);

		eqv(p, "1@1/2", LispComplex.newPolar(newZ(1), newQ(1, 2)));
		eq(p, "1@+1/2", LispComplex.newPolar(newZ(1), newQ(1, 2)));
		eq(p, "1@-1/2", LispComplex.newPolar(newZ(1), newQ(-1, 2)));
		eq(p, "1@3/3", LispComplex.newPolar(newZ(1), newZ(1)));
	}

	public void testComplex() throws IOException {
		Parser p = getParser();

		eqcmp(p, "1+0.7i", 1.0, 0.7);   eqcmp(p, "1-0.7i", 1.0, -0.7);
		eqcmp(p, "1+.7i", 1.0, 0.7);    eqcmp(p, "1-.7i", 1.0, -0.7);
		eqcmpE(p, "1+1i", 1, 1);        eqcmpE(p, "1-1i", 1, -1);
		eqcmpE(p, "1+i", 1, 1);         eqcmpE(p, "1-i", 1, -1);
		eqcmp(p, "1+1e+1i", 1.0, 1e1);  eqcmp(p, "1+1e-1i", 1.0, 1e-1);

		eqcmpE(p, "1+1/2i", newZ(1), newQ(1, 2));
		eqcmpE(p, "1-1/2i", newZ(1), newQ(-1, 2));
		eqcmpE(p, "1+3/3i", 1, 1);
	}

	public void testInfinity() throws IOException {
		Parser p = getParser();

		eqdbl(p, "+inf.0", Double.POSITIVE_INFINITY);
		eqdbl(p, "-inf.0", Double.NEGATIVE_INFINITY);
		eqimg(p, "+inf.0i", Double.POSITIVE_INFINITY);
		eqimg(p, "-inf.0i", Double.NEGATIVE_INFINITY);
		eqcmp(p, "1+inf.0i", 1.0, Double.POSITIVE_INFINITY);
		eqcmp(p, "1-inf.0i", 1.0, Double.NEGATIVE_INFINITY);
		eqcmp(p, "+inf.0+inf.0i",
				Double.POSITIVE_INFINITY, Double.POSITIVE_INFINITY);
		eqcmp(p, "-inf.0-inf.0i",
				Double.NEGATIVE_INFINITY, Double.NEGATIVE_INFINITY);
		ok(Double.isNaN(((LispDouble)read(p, "+nan.0")).doubleValue()));
	}

	public void testRadix() throws IOException {
		Parser p = getParser();

		eqv(p, "#b-10", newZ(-2));
		eqv(p, "#b10", newZ(2));
		eqv(p, "#b10/11", newQ(2, 3));
		try {
			eqv(p, "#b12", F);
		} catch(ReadException e) { }

		eqv(p, "#o10", newZ(8));
		eqv(p, "#o10/11", newQ(8, 9));
		eqv(p, "#o17", newZ(15));
		try {
			eqv(p, "#o18", F);
		} catch(ReadException e) { }

		eqv(p, "#d10", newZ(10));
		eqv(p, "#d10/11", newQ(10, 11));
		eqv(p, "#d17", newZ(17));
		eqv(p, "#d19", newZ(19));
		try {
			eq(p, "#b1a", F);
		} catch(ReadException e) { }

		eqv(p, "#x10", newZ(16));
		eqv(p, "#x10/11", newQ(16, 17));
		eqv(p, "#x17", newZ(23));
		eqv(p, "#x19", newZ(25));
		eqv(p, "#x1f", newZ(31));
		try {
			eq(p, "#x1g", F);
		} catch(ReadException e) { }
	}

	public void testExactness() throws IOException {
		Parser p = getParser();

		eqdbl(p, "#i1", 1.0);
		eqdbl(p, "#i-1", -1.0);
		eqdbl(p, "#i1/16", 0.0625);
		eqdbl(p, "#i#d1/16", 0.0625);
		eqdbl(p, "#i#b1/10", 0.5);
		eqdbl(p, "#i#o1/10", 0.125);
		eqdbl(p, "#i#x1/10", 0.0625);
		eqdbl(p, "#d#i1/16", 0.0625);
		eqdbl(p, "#b#i1/10", 0.5);
		eqdbl(p, "#o#i1/10", 0.125);
		eqdbl(p, "#x#i1/10", 0.0625);
		eqcmp(p, "#i1/2+1/2i", 0.5, 0.5);

		eqrat(p, "#e0.5", 1, 2);
		eqrat(p, "#e-0.5", -1, 2);
		eqint(p, "#e1.0", 1);
		eqint(p, "#e1e1", 10);
		eqrat(p, "#e1e-1", 1, 10);
		eqrat(p, "#e#d1/16", 1, 16);
		eqrat(p, "#e#b1/10000", 1, 16);
		eqrat(p, "#e#o1/20", 1, 16);
		eqrat(p, "#e#x1/10", 1, 16);
		eqrat(p, "#d#e1/16", 1, 16);
		eqrat(p, "#b#e1/10000", 1, 16);
		eqrat(p, "#o#e1/20", 1, 16);
		eqrat(p, "#x#e1/10", 1, 16);

		rderr(p, "#i#e1");  rderr(p, "#e#i1");
		rderr(p, "#b#o1");  rderr(p, "#b#d1");  rderr(p, "#b#x1");
		rderr(p, "#o#b1");  rderr(p, "#o#d1");  rderr(p, "#o#x1");
		rderr(p, "#d#b1");  rderr(p, "#d#o1");  rderr(p, "#d#x1");
		rderr(p, "#x#b1");  rderr(p, "#x#o1");  rderr(p, "#x#d1");
		rderr(p, "#i1i");
	}

	public void testString() throws IOException {
		Parser p = getParser();

		eqstr(p, "\"string\"", "string");
		eqstr(p, "\"\\\\\\\"\"", "\\\"");

		p.clear();
		ng(p.readParse("\"line1"));
		ng(p.readParse("line2\\"));
		ok(p.readParse("end\""));
		eqstr(p.getDatum(), str("line1\nline2end"));
	}

	public void testCharacter() throws IOException {
		Parser p = getParser();

		eqv(p, "#\\a", chr('a'));
		eqv(p, "#\\newline", chr('\n'));
		eqv(p, "#\\space", chr(' '));
		rderr(p, "#\\unknown");
	}

	public void testList() throws IOException {
		Parser p = getParser();

		eqv(p, "()", Nil.NIL);
		equal(p, "(1 2 3)", list(1, 2, 3));
		equal(p, "(1 . ())", list(1));
		equal(p, "(1 . (2 . ()))", list(1, 2));
		equal(p, "(1 .(2 .()))", list(1, 2));
		equal(p, "(1 . 2)", cons(1, 2));
		equal(p, "((#\\a 2) (c \"d\" . 5))",
				list(list('a', 2), listDot(5, sym("c"), str("d"))));
		equal(p, "(1 .5 .2)", list(1, .5, .2));
		equal(p, "(1 .5 . 2)", listDot(2, 1, .5));
		rderr(p, "(1 . 5 . 2)");
		rderr(p, "(1 . 5 .2)");
		rderr(p, "(1 . )");
		rderr(p, "(1 .)");
	}

	public void testVector() throws IOException {
		Parser p = getParser();

		equal(p, "#()", vec());
		equal(p, "#(1 2 3)", vec(1, 2, 3));
		equal(p, "#(#(#\\a 2) #(c \"d\"))",
				vec(vec('a', 2), vec(sym("c"), str("d"))));
		equal(p, "#(1 .5 .2)", vec(1, .5, .2));

		equal(p, "#((#\\a 2) (c \"d\" . 5))",
				vec(list('a', 2), listDot(5, sym("c"), str("d"))));
	}

	public void testQuote() throws IOException {
		Parser p = getParser();

		equal(p, "'a", qt(QT, sym("a")));
		equal(p, "'(a 'b)", qt(QT, list(sym("a"), qt(QT, sym("b")))));
		equal(p, "'''a", qt(QT, qt(QT, qt(QT, sym("a")))));
		equal(p, "`(a ,b)", qt(QQT, list(sym("a"), qt(UQT, sym("b")))));
		equal(p, "`(,@b)", qt(QQT, list(qt(UQTS, sym("b")))));
	}

	public void testComment() throws IOException {
		Parser p = getParser();

		//p.clear();  ng(p.readParse(";comment"));
		//p.clear();  ng(p.readParse("#| comment |#"));
		//p.clear();  ng(p.readParse("#| nested #| comment |# . |#"));

		p.clear();
		ng(p.readParse("(1 2 ; 3)"));
		ok(p.readParse("4)"));
		equal(p.getDatum(), list(1, 2, 4));
		p.clear();
		ng(p.readParse("#(1 2 ; 3)"));
		ok(p.readParse("4)"));
		equal(p.getDatum(), vec(1, 2, 4));

		eq   (p, "(#; 'a)", Nil.NIL);
		equal(p, "(#; (a b) 1 2)", list(1, 2));
		equal(p, "(1 . #;2 3)", cons(1, 3));
		rderr(p, "(#;1 . 2)");
		rderr(p, "(1 . #;2)");
		equal(p, "#(#;1)", vec());
		equal(p, "#(#; (a b) 1 2)", vec(1, 2));
	}

	public void testParenthesis() throws IOException {
		Parser p = getParser();

		eq   (p, "[]", Nil.NIL);
		equal(p, "(1 2[3 4]5)", list(1, 2, list(3, 4), 5));
		equal(p, "[1 . 2]", cons(1, 2));
		equal(p, "[1.2]", list(1.2));

		rderr(p, "(]");
		rderr(p, "(1 2(3 4)5]");
		rderr(p, "(1 . 2]");
		rderr(p, "[1 2(3 4)5)");
		rderr(p, "[1 . 2)");
	}

	public void testSharpSyntax() throws IOException {
		Parser p = getParser();

		eq   (p, "#t", T);
		eq   (p, "#f", F);
		rderr(p, "#unknown");
	}

}
