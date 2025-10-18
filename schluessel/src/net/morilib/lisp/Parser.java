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
package net.morilib.lisp;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import net.morilib.lang.Decimal32;
import net.morilib.lang.Decimal64;
import net.morilib.lang.DoubleUtils;
import net.morilib.lang.number.Rational;
import net.morilib.lisp.arith.LispRatio;
import net.morilib.lisp.charset.LispCharSet;
import net.morilib.lisp.collection.LispHashSet;
import net.morilib.lisp.math.probability.LispProbability;
import net.morilib.lisp.phys.unit.LispQuantity;
import net.morilib.lisp.phys.unit.LispUnitSystem;
import net.morilib.lisp.topology.LispRealNumberSet;
import net.morilib.phys.unit.Quantity;
import net.morilib.phys.unit.Units;
import net.morilib.range.Interval;
import net.morilib.range.Intervals;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class Parser {

	//
	private static enum State {
		INIT,                           // 初期状態
		NUMBER_PREFIX,                  // 数値のprefix
		HEAD_SIGN,                      // 先頭の符号
		NUMBER,                         // 整数
		FLOAT_NUMBER,                   // 浮動小数点開始
		FLOAT_NUMBER_D,                 // 浮動小数点
		FLOAT_NUMBER_CYCLE,
		FLOAT_NUMBER_ELLIPSE1,
		FLOAT_NUMBER_ELLIPSE2,
		FLOAT_NUMBER_E,                 // 浮動小数点指数部開始
		FLOAT_NUMBER_E_SIGN,            // 浮動小数点指数部符号
		FLOAT_NUMBER_E_D,               // 浮動小数点指数部
		RATIONAL_NUMBER,                // 有理数開始
		RATIONAL_NUMBER_2,              // 有理数
		CONTINUED_FRACTION,
		INF_I,                          // 無限
		INF_N,
		INF_F,
		INF_DOT,
		INF_0,
		COMPLEX_I,                      // 純虚数
		INIT_POLAR,                     // 初期状態(極形式)
		HEAD_SIGN_POLAR,                // 先頭の符号(極形式)
		NUMBER_POLAR,                   // 整数(極形式)
		FLOAT_NUMBER_POLAR,             // 浮動小数点開始(極形式)
		FLOAT_NUMBER_D_POLAR,           // 浮動小数点(極形式)
		FLOAT_NUMBER_E_POLAR,           // 浮動小数点指数部開始(極形式)
		FLOAT_NUMBER_E_SIGN_POLAR,      // 浮動小数点指数部符号(極形式)
		FLOAT_NUMBER_E_D_POLAR,         // 浮動小数点指数部(極形式)
		RATIONAL_NUMBER_POLAR,          // 有理数開始(極形式)
		RATIONAL_NUMBER_2_POLAR,        // 有理数(極形式)
		HEAD_SIGN_COMPLEX,              // 先頭の符号(複素数)
		NUMBER_COMPLEX,                 // 整数(複素数)
		FLOAT_NUMBER_COMPLEX,           // 浮動小数点開始(複素数)
		FLOAT_NUMBER_COMPLEX_CYCLE,
		FLOAT_NUMBER_COMPLEX_CYCLE2,
		FLOAT_NUMBER_COMPLEX_ELLIPSE1,
		FLOAT_NUMBER_COMPLEX_ELLIPSE2,
		FLOAT_NUMBER_COMPLEX_ELLIPSE3,
		FLOAT_NUMBER_D_COMPLEX,         // 浮動小数点(複素数)
		FLOAT_NUMBER_E_COMPLEX,         // 浮動小数点指数部開始(複素数)
		FLOAT_NUMBER_E_SIGN_COMPLEX,    // 浮動小数点指数部符号(複素数)
		FLOAT_NUMBER_E_D_COMPLEX,       // 浮動小数点指数部(複素数)
		RATIONAL_NUMBER_COMPLEX,        // 有理数開始(複素数)
		RATIONAL_NUMBER_2_COMPLEX,      // 有理数(複素数)
		CONTINUED_FRACTION_COMPLEX,
		INF_I_COMPLEX,                  // 無限(複素数)
		INF_N_COMPLEX,
		INF_F_COMPLEX,
		INF_DOT_COMPLEX,
		INF_0_COMPLEX,
		COMPLEX_I2,                     // 複素数単位(i)
		STRING,                         // 文字列
		STRING_ESCAPE,                  // 文字列(エスケープ)
		STRING_CODE,
		STRING_NEWLINE,
		STRING_NEWLINE2,
		SHARP_SYNTAX_I,                 // #書式(初回)
		SHARP_SYNTAX,                   // #書式
		SHARP_SYNTAX_RE,                // #書式(正規表現)
		SHARP_SYNTAX_RE_F,              // #書式(正規表現フラグ)
		SHARP_HASH_BANG,
		CHARACTER_EXP,                  // 文字
		CHARACTER_EXP_NAME,             // 文字(コード)
		SYMBOL,                         // シンボル
		SYMBOL2,                        // シンボル(複素数)
		LIST,                           // リスト(先頭)
		LIST2,                          // リスト(途中)
		LIST_DOT_INIT,                  // ドットリスト(INIT)
		LIST_DOT,                       // ドットリスト
		QUOTE,                          // quote("'", "`", ",", ",@")
		VECTOR,                         // ベクタ(先頭)
		VECTOR2,                        // ベクタ(途中)
		COMMENT,                        //
		BLOCK_COMMENT,                  //
		S_EXP_COMMENT,                  //
		S_EXP_MACRO,
		S_EXP_QUOTE,
		END_OF_FILE,
		SHARP_SYNTAX_SRFI38_1,
		SHARP_SYNTAX_SRFI38_2,
		SYMBOLQUOTE,
		SYMBOLQUOTE2,
		SHARP_SYNTAX_RANGE,
		SHARP_SYNTAX_CHARSET,
		RANGE,
		RANGE2,
		RANGE3,
		//END_TOKEN,
	};

	//
	private enum Exact {
		DEFAULT,
		EXACT,
		INEXACT,
	}

	//
	private static class DumMarker extends Datum {

		//
		private Object number;

		//
		private DumMarker(Object num) {
			this.number = num;
		}

		//
		public boolean equals(Object o) {
			if(o instanceof DumMarker) {
				return number.equals(((DumMarker)o).number);
			}
			return false;
		}

		//
		public int hashCode() {
			return number.hashCode();
		}

	}

	// marker
	private static final Datum _VOID = new Datum() {};
	private static final Symbol _DOT = Symbol.getSymbol(".");
	private static final int EOF = -1;
	private static final Datum _STK_CONS = Symbol.gensym();
	private static final Datum _STK_VEC  = Symbol.gensym();
	private static final int I_BIT = 0x1;
	//private static final int J_BIT = 0x2;
	//private static final int K_BIT = 0x3;
	private static final int L_BIT = 0x4;

	//
	private LispMessage message;
	private ReadUnreadable rd = null;
	private State state;
	private StringBuilder buf  = new StringBuilder();
	private StringBuilder buf2 = new StringBuilder();  // for imaginary
	private StringBuilder buf3 = new StringBuilder();  // for symbol
	private StringBuilder[] bufi = new StringBuilder[8];
	private Stack<Datum>       resultStack = new Stack<Datum>();
	private Stack<State>       stateStack  = new Stack<State>();
	private Stack<Cons>        consStack   = new Stack<Cons>();
	private Stack<List<Datum>> vectorStack = new Stack<List<Datum>>();
	private Stack<Integer>     parenStack  = new Stack<Integer>();
	private Stack<Datum>       rangeStack  = new Stack<Datum>();
	private boolean prefixed = false;
	private Exact exactness = Exact.DEFAULT;
	private int radix = -1;
	private boolean signed = false;
	private boolean readBefore = false;
	private boolean consoleMode;
	private Map<Object, Datum> definedDatum =
		new HashMap<Object, Datum>();
	private Set<Object>        nowNumber   = new HashSet<Object>();
	private Stack<Set<Object>> numberStack = new Stack<Set<Object>>();

	//private Environment readerenv;   // for read macro

	//
	/*package*/ Parser(LispMessage msg) {
		message = msg;
		consoleMode = true;
		//readerenv = null;
		clear();
	}

	//
	/*package*/ Parser(Environment env, LispMessage msg) {
		message = msg;
		consoleMode = true;
		//readerenv = env;
		clear();
	}

	//
	/*package*/ Parser(InputPort read, LispMessage msg) {
		message = msg;
		consoleMode = false;
		rd = read;
		//readerenv = null;
		clear();
	}

	/**
	 * 
	 */
	public Parser() {
		this(LispMessage.getInstance());
	}

	/**
	 * 
	 * @param lc
	 */
	public Parser(Locale lc) {
		this(LispMessage.getInstance(lc));
	}

	/**
	 * @param lispTextualInputPort
	 */
	public Parser(ReadUnreadable rd) {
		this.rd = rd;
	}

	/**
	 * 
	 * @param rd
	 * @return
	 * @throws IOException
	 */
	public static List<Datum> readSExpression(
			Reader rd) throws IOException {
		Parser p;
		LispMessage msg = LispMessage.getInstance();

		p = new Parser(new InputPort(rd, msg), msg);
		while(p.parse() && !p.state.equals(State.END_OF_FILE)) {}
		return p.getData();
	}

	//
	private void clearBuf() {
		buf  = new StringBuilder();
		buf2 = new StringBuilder();
		buf3 = new StringBuilder();
		prefixed = false;
		exactness = Exact.DEFAULT;
		radix = -1;
		signed = false;
		Arrays.fill(bufi, null);
	}

	//
	private int read() throws IOException {
		int res = rd.getc();

		if(res < 0) {
			return EOF;
		} else {
			readBefore = true;
			return res;
		}
	}

	//
	private int readNext2() throws IOException {
		int res;

		while((res = rd.getc()) >= 0) {
			readBefore = true;
			if(!Character.isWhitespace(res)) {
				return res;
			}
		}
		return EOF;
	}

	//
	private int readNext() throws IOException {
		clearBuf();
		return readNext2();
	}

	//
	private void appendBuf(int c) {
//		buf.append((char)c);
		buf.appendCodePoint(c);
	}

	//
	private void appendBuf2(int c) {
		buf2.append((char)c);
		buf3.append((char)c);
	}

	//
	private String getToken() {
		String res = buf.toString();
		return res;
	}

	//
	private String getTokenWithClear() {
		String res = buf.toString();

		buf = new StringBuilder();
		return res;
	}

	//
	private String getToken2() {
		String res = buf2.toString();
		return res;
	}

	//
	private static double parseDouble(String str) {
		if("+inf.0".equals(str)) {
			return Double.POSITIVE_INFINITY;
		} else if("-inf.0".equals(str)) {
			return Double.NEGATIVE_INFINITY;
		} else if("+nan.0".equals(str)) {
			return Double.NaN;
		} else if("-nan.0".equals(str)) {
			return Double.NaN;
		} else {
			return Double.parseDouble(str);
		}
	}

	//
//	private static double getValueByDouble(String str) {
//		double i;
//		String[] ind = str.split("/");
//
//		if(ind.length == 2) {
//			double n = parseDouble(ind[0]);
//			double d = parseDouble(ind[1]);
//
//			i = n / d;
//		} else if(ind.length == 1) {
//			i = parseDouble(ind[0]);
//		} else {
//			throw new RuntimeException("internal error");
//		}
//		return i;
//	}

	//
	private static BigInteger getValueByBigInt(String str, int r) {
		return new BigInteger(str.replaceFirst("^\\+", ""), r);
	}

	//
	private boolean isExpsym(int r) {
		return (r == 'e' || r == 'E' ||
				r == 's' || r == 'S' ||
				r == 'f' || r == 'F' ||
				r == 'd' || r == 'D' ||
				r == 'l' || r == 'L');
	}

	//
	private boolean isDigit09az(int r) {
		return ((r >= '0' && r <= '9') ||
				(radix == 16 && ((r >= 'A' && r <= 'F') ||
						(r >= 'a' && r <= 'f'))));
	}

	//
	private boolean isDigit09(int r) {
		return (r >= '0' && r <= '9');
	}

	//
	private boolean isImaginary(int r) {
		return r == 'i' || r == 'j' || r == 'k' || r == 'o';
	}

	//
	private int getImaginaryNo(int r) {
		switch(r) {
		case 'i':  return 1;
		case 'j':  return 2;
		case 'k':  return 3;
		case 'o':  return 4;
		default:   throw new RuntimeException();
		}
	}

	//
	private static String reverseNumFormat(
			Exact exactness, int radix) {
		String res = "";

		if(Exact.EXACT.equals(exactness)) {
			res += "#e";
		} else if(Exact.INEXACT.equals(exactness)) {
			res += "#i";
		}

		if(radix == 2) {
			res += "#b";
		} else if(radix == 8) {
			res += "#o";
		} else if(radix == 10) {
			res += "#d";
		} else if(radix == 16) {
			res += "#x";
		}
		return res;
	}

	//
	private String reverseNumFormat() {
		return reverseNumFormat(exactness, radix);
	}

	//
	private State symbolNum() {
		if(prefixed) {
			//String s =
			//	reverseNumFormat() + getToken() + getToken2();

			throw message.getReadError(
					"err.read.format.numeric");
			//throw new ReadException("bad numeric format");
		}
		return State.SYMBOL;
	}

	//
	private State symbol2Num() {
		if(prefixed) {
			//String s =
			//	reverseNumFormat() + getToken() + getToken2();

			throw message.getReadError(
					"err.read.format.numeric");
			//throw new ReadException("bad numeric format");
		}
		return State.SYMBOL2;
	}

	//
	private State popStateSymbolNum() {
		if(prefixed) {
			throw message.getReadError(
					"err.read.format.numeric",
					reverseNumFormat() + getToken() + getToken2());
			//throw new ReadException("bad numeric format");
		}
		pushMaybeSymbol(getSymbol());
		return popStateStack();
	}

	//
	private LispNumber getInteger() {
		String str = getToken();
		BigInteger val;
		LispReal atom;
		int r = (radix < 0) ? 10 : radix;
		Exact ex = exactness;

		clearBuf();
		try {
			val = getValueByBigInt(str, r);
			if(Exact.INEXACT.equals(ex)) {
				atom = new LispDouble(val.doubleValue());
			} else {
				atom = LispInteger.valueOf(val);
			}
			return atom;
		} catch(NumberFormatException e) {
			throw message.getReadError(
					"err.read.format.numeric",
					reverseNumFormat(ex, radix) + str);
			//throw new ReadException("number format error");
		}
	}

	//
//	private static final Pattern PTN_CF = Pattern.compile(
//			"(\\+|-)?([0-9]+(\\+|-))?[0-9]+(/[0-9]+(\\+|-))+/[0-9]+");
	private static final Pattern PTN_B0 = Pattern.compile(
			"((\\+|-)?[0-9]+)?((\\+|-)?[0-9]+)");

	//
	private static String chopsign(String s) {
		return "+-".indexOf(s.charAt(s.length() - 1)) < 0 ?
				s : s.substring(0, s.length() - 1);
	}

	/**
	 * 
	 * @param str
	 * @return
	 */
	public static LispReal parseContinuedFraction(
			String str, int r, Exact ex) {
		String[] spt = str.split("/");
		Matcher  mt0 = PTN_B0.matcher(spt[0]);
		Rational b0, a1, b1, d, dh, h, b;
		String   s0, s1;
		int sg;

		if(!mt0.matches())  throw new RuntimeException();
		s0 = mt0.group(1);
		s1 = mt0.group(3);
		b0 = (s0 == null || s0.equals("")) ?
				Rational.ZERO : Rational.valueOf(new BigInteger(s0));
		a1 = Rational.valueOf(getValueByBigInt(s1, r));
		sg = spt[1].charAt(spt[1].length() - 1) == '+' ? 1 : -1;
		b1 = Rational.valueOf(getValueByBigInt(chopsign(spt[1]), r));
		d  = b1.invert();
		dh = a1.divide(b1);
		h  = b0.add(dh);
		for(int i = 2; i < spt.length; i++) {
			b  = Rational.valueOf(
					getValueByBigInt(chopsign(spt[i]), r));
			d  = b.multiply(sg).add(d).invert();
			dh = dh.multiply(b.multiply(d).subtract(Rational.ONE));
			h  = h.add(dh);
			sg = spt[i].charAt(spt[i].length() - 1) == '+' ? 1 : -1;
		}
		return Exact.INEXACT.equals(ex) ?
				new LispDouble(h.doubleValue()) :
					LispRational.valueOf(h);
	}

	//
	private static LispReal getSimpleRational(String str,
			int r, Exact ex, LispMessage message) {
		BigInteger num, den;
//		String str = getToken();
		String[] nd = str.split("/");
//		int r = (radix < 0) ? 10 : radix;
//		Exact ex = exactness;

//		clearBuf();
		try {
			num = getValueByBigInt(nd[0], r);
			den = getValueByBigInt(nd[1], r);
			if(Exact.INEXACT.equals(ex)) {
				double numd = num.doubleValue();
				double dend = den.doubleValue();

				return new LispDouble(numd / dend);
			} else {
				return LispRational.newRational(num, den);
			}
		} catch(NumberFormatException e) {
			throw message.getReadError(
					"err.read.format.numeric",
					reverseNumFormat(ex, r) + str);
			//throw new ReadException("number format error");
		}
	}

	//
	private LispReal getRational(String str) {
		int r = (radix < 0) ? 10 : radix;
		Exact ex = exactness;

		if(str.indexOf("+/") >= 0 || str.indexOf("-/") >= 0) {
			return parseContinuedFraction(str, r, ex);
		} else {
			return getSimpleRational(str, r, ex, message);
		}
	}

	//
	private LispReal getRational() {
		LispReal r;
		String tok = getToken();

		r = getRational(tok);
		clearBuf();
		return r;
	}

	//
	private static LispReal computeCycle(
			String fix, String cyc, int fixd) {
		BigDecimal d1, d2, d3;
		BigInteger i1;
		LispReal r1;

		d1  = new BigDecimal(BigInteger.ONE, -cyc.length());
		d1  = d1.subtract(BigDecimal.ONE);
		i1  = new BigInteger(cyc);
		r1  = LispRational.newRational(i1, d1.toBigInteger());
		d2  = new BigDecimal(BigInteger.ONE, fixd);
		r1  = r1.multiply(LispUtils.bigDecimalToRational(d2));
		d3  = new BigDecimal(fix);
		r1  = r1.add(LispUtils.bigDecimalToRational(d3));
		return r1;
	}

	//
	private static LispReal getCycle(String str) {
		String cyc;
		int l, p;

		p = str.indexOf('.');
		cyc = str.substring((l = str.indexOf('{')) + 1);
		cyc = cyc.replaceAll("}", "");
		str = str.substring(0, l) + "0";
		return computeCycle(str, cyc, l - p - 1);
	}

	//
	private static String findLastLongestRepeat(CharSequence c) {
		final int len = c.length() - 1;
		final int max = c.length() / 2;
		char[] cs = new char[max];
		String s, t;

		outer: for(int i = max; i > 0; i--) {
			for(int j = 0; j < i; j++) {
				if(c.charAt(len - j) == c.charAt(len - j - i)) {
					cs[max - j - 1] = c.charAt(len - j);
				} else {
					continue outer;
				}
			}

			if((t = findLastLongestRepeat(
					s = new String(cs, max - i, i))) == null) {
				return s;
			} else {
				return t;
			}
		}
		return null;
	}

	//
	private LispReal getEllipseN(String str) {
		String s = str.substring(0, str.length() - 1);
		String cyc = findLastLongestRepeat(s), fix;

		if(cyc != null) {
			fix = s.replaceAll(cyc, "");
		} else {
			cyc = s.substring(s.length() - 1);
			fix = s.substring(0, s.length() - 1);
		}
		return computeCycle(fix + "0", cyc,
				fix.length() - str.indexOf('.') - 1);
	}

	//
	private LispReal getDouble(String str) {
		int r = (radix < 0) ? 10 : radix;
		Exact ex = exactness;

		if(r != 10) {
			throw message.getReadError(
					"err.read.notsupported.radix10",
					reverseNumFormat(ex, r) + str);
			//throw new ReadException("only radix 10 is supported");
		}

		if(str.contains("{")) {
			return Exact.INEXACT.equals(ex) ?
					getCycle(str).toInexact() : getCycle(str);
		} else if(str.endsWith(".")) {
			return Exact.INEXACT.equals(ex) ?
					getEllipseN(str).toInexact() : getEllipseN(str);
		} else if(Exact.EXACT.equals(ex)) {
			//return atom.toExact();
			return LispUtils.bigDecimalToRational(new BigDecimal(str));
		} if("+inf.0".equals(str)) {
			return LispDouble.POSITIVE_INFINITY;
		} else if("-inf.0".equals(str)) {
			return LispDouble.NEGATIVE_INFINITY;
		} else if("+nan.0".equals(str)) {
			return LispDouble.NaN;
		} else if("-nan.0".equals(str)) {
			return LispDouble.NaN;
		} else if(str.indexOf('s') >= 0) {
			return new LispHalf(DoubleUtils.toHalf(
					(float)parseDouble(str.replace('s', 'e'))));
		} else if(str.indexOf('f') >= 0) {
			return new LispFloat((float)parseDouble(
					str.replace('f', 'e')));
		} else if(str.indexOf('d') >= 0) {
			return new LispDouble(parseDouble(str.replace('d', 'e')));
		} else if(str.indexOf('l') >= 0) {
			return new LispDouble(parseDouble(str.replace('l', 'e')));
		} else if(str.indexOf('#') >= 0) {
			return new LispDecimal32(Decimal32.parseDecimal(
					str.replace('#', 'e')));
		} else if(str.indexOf('&') >= 0) {
			return new LispDecimal64(Decimal64.parseDecimal(
					str.replace('&', 'e')));
		} else {
			return new LispDouble(parseDouble(str));
		}
	}

	//
	private LispReal getDouble() {
		LispReal n = getDouble(getToken());

		clearBuf();
		return n;
	}

	//
	private LispNumber getImaginary(int imgn) {
		String gtok1 = getToken();
		int rx = (radix < 0) ? 10 : radix;
		Exact ex = exactness;
		String iii = gtok1.replaceFirst("[ijko][ijk]?$", "");
		LispReal im;

		clearBuf();
		if(rx > 0 && rx != 10) {
			throw message.getReadError(
					"err.read.notsupported.radix10",
					reverseNumFormat(ex, rx) + gtok1);
		}

		// imaginary part
		if("+".equals(iii)) {
			im = !Exact.INEXACT.equals(ex) ?
					LispInteger.ONE : new LispDouble(1.0);
		} else if("-".equals(iii)) {
			im = !Exact.INEXACT.equals(ex) ?
					LispInteger.ONE.uminus() : new LispDouble(-1.0);
		} else if(iii.indexOf('/') >= 0) {
//			BigInteger num, den;
//			String[] nd = iii.split("/");
//
//			try {
//				num = getValueByBigInt(nd[0], rx);
//				den = getValueByBigInt(nd[1], rx);
//				if(Exact.INEXACT.equals(ex)) {
//					double numd = num.doubleValue();
//					double dend = den.doubleValue();
//
//					im = new LispDouble(numd / dend);
//				} else {
//					im = LispRational.newRational(num, den);
//				}
//			} catch(NumberFormatException e) {
//				throw message.getReadError(
//						"err.read.format.numeric",
//						reverseNumFormat(ex, rx) + gtok1);
//			}
			im = getRational(iii);
		} else if(!Exact.INEXACT.equals(ex) &&
				iii.matches("^[-+]?[0-9]+$")) {
			iii = iii.replaceFirst("^\\+", "");
			im = LispInteger.valueOf(getValueByBigInt(iii, rx));
		} else {
			im = getDouble(iii);
		}

		switch(imgn) {
		case 1:
			return LispComplex.newComplex(LispInteger.ZERO, im);
		case 2:
			return LispQuaternion.newQuaternion(
					LispInteger.ZERO, LispInteger.ZERO,
					im, LispInteger.ZERO);
		case 3:
			return LispQuaternion.newQuaternion(
					LispInteger.ZERO, LispInteger.ZERO,
					LispInteger.ZERO, im);
		case 4:
			return LispOctonion.newOctonion(
					LispInteger.ZERO, LispInteger.ZERO,
					LispInteger.ZERO, LispInteger.ZERO,
					im, LispInteger.ZERO,
					LispInteger.ZERO, LispInteger.ZERO);
		case 5:
			return LispOctonion.newOctonion(
					LispInteger.ZERO, LispInteger.ZERO,
					LispInteger.ZERO, LispInteger.ZERO,
					LispInteger.ZERO, im,
					LispInteger.ZERO, LispInteger.ZERO);
		case 6:
			return LispOctonion.newOctonion(
					LispInteger.ZERO, LispInteger.ZERO,
					LispInteger.ZERO, LispInteger.ZERO,
					LispInteger.ZERO, LispInteger.ZERO,
					im, LispInteger.ZERO);
		case 7:
			return LispOctonion.newOctonion(
					LispInteger.ZERO, LispInteger.ZERO,
					LispInteger.ZERO, LispInteger.ZERO,
					LispInteger.ZERO, LispInteger.ZERO,
					LispInteger.ZERO, im);
		default:
			throw new RuntimeException("" + imgn);
		}
	}

	//
	private LispNumber getComplex() {
		String gtok1 = getToken(), iii;
		int    rx = (radix < 0) ? 10 : radix;
		Exact  ex = exactness;
		LispReal re;
		LispReal[] im = new LispReal[8];

		if(rx > 0 && rx != 10) {
			throw message.getReadError(
					"err.read.notsupported.radix10");
		}

		// real part
		if(gtok1.indexOf('/') >= 0) {
//			BigInteger num, den;
//			String[] nd = gtok1.split("/");
//
//			try {
//				num = getValueByBigInt(nd[0], rx);
//				den = getValueByBigInt(nd[1], rx);
//				if(Exact.INEXACT.equals(ex)) {
//					double numd = num.doubleValue();
//					double dend = den.doubleValue();
//
//					re = new LispDouble(numd / dend);
//				} else {
//					re = LispRational.newRational(num, den);
//				}
//			} catch(NumberFormatException e) {
//				throw message.getReadError(
//						"err.read.format.numeric",
//						reverseNumFormat(ex, rx) + gtok1);
//			}
			re = getRational(gtok1);
		} else if(!Exact.INEXACT.equals(ex) &&
				gtok1.matches("^[-+]?[0-9]+$")) {
			re = LispInteger.valueOf(getValueByBigInt(gtok1, rx));
		} else {
			re = getDouble(gtok1);
		}

		// imaginary part
		for(int i = 1; i < 8; i++) {
			if(bufi[i] == null) {
				im[i] = LispInteger.ZERO;
				continue;
			}

			iii = bufi[i].toString().replaceFirst("[ijko][ijk]?$", "");
			if("+".equals(iii)) {
				im[i] = !Exact.INEXACT.equals(ex) ?
						LispInteger.ONE : new LispDouble(1.0);
			} else if("-".equals(iii)) {
				im[i] = !Exact.INEXACT.equals(ex) ?
						LispInteger.ONE.uminus() : new LispDouble(-1.0);
			} else if(iii.indexOf('/') >= 0) {
//				BigInteger num, den;
//				String[] nd = iii.split("/");
//	
//				try {
//					num = getValueByBigInt(nd[0], rx);
//					den = getValueByBigInt(nd[1], rx);
//					if(Exact.INEXACT.equals(ex)) {
//						double numd = num.doubleValue();
//						double dend = den.doubleValue();
//	
//						im[i] = new LispDouble(numd / dend);
//					} else {
//						im[i] = LispRational.newRational(num, den);
//					}
//				} catch(NumberFormatException e) {
//					throw message.getReadError(
//							"err.read.format.numeric",
//							reverseNumFormat(ex, rx) + iii);
//				}
				im[i] = getRational(iii);
			} else if(!Exact.INEXACT.equals(ex) &&
					iii.matches("^[-+]?[0-9]+$")) {
				iii = iii.replaceFirst("^\\+", "");
				im[i] = LispInteger.valueOf(getValueByBigInt(iii, rx));
			} else {
				im[i] = getDouble(iii);
			}
		}

		clearBuf();
		return LispOctonion.newOctonion(
				re,    im[1], im[2], im[3],
				im[4], im[5], im[6], im[7]);
	}

	//
	private LispNumber getPolar() {
		String str = getToken();
//		double r;
		LispReal r, a;
		int rx = (radix < 0) ? 10 : radix;
		Exact ex = exactness;

		clearBuf();
		if(rx > 0 && rx != 10) {
			throw message.getReadError(
					"err.read.notsupported.radix10",
					reverseNumFormat(ex, rx) + str);
			//throw new ReadException("only radix 10 is supported");
		}

		String[] ra = str.split("@");
//		r = getValueByDouble(ra[0]);    // 半径
//		if(r == 0.0) {
//			return new LispDouble(0.0);
//		} else {
//			double a = getValueByDouble(ra[1]);
//
//			return LispComplex.newComplex(
//					r * Math.cos(a), r * Math.sin(a));
//		}
		r = LispNumber.parse(ra[0], rx).getReal();
		a = LispNumber.parse(ra[1], rx).getReal();
		return LispComplex.newPolar(r, a);
	}

	//
	private Datum getRegex() {
		String  tok = getToken();
		Matcher mch = REGEX_PAT.matcher(tok);
		mch.find();

		String  re  = mch.group(1);
		String  fl  = mch.group(2);

		try {
			return new RegexPattern(re, fl);
		} catch(PatternSyntaxException e) {
			throw message.getReadError(
					"err.read.regex.syntax", re);
		}
	}

	//
	private Pattern REGEX_PAT = Pattern.compile("/(.*)/([iuc]*)");

	/*private Datum getSharpSyntax(boolean sexp) {
		String str = getToken();
		Matcher mch;

		clearBuf();
		if("t".equalsIgnoreCase(str)) {
			return LispBoolean.TRUE;
		} else if("f".equalsIgnoreCase(str)) {
			return LispBoolean.FALSE;
		} else if((mch = REGEX_PAT.matcher(str)).matches()) {
			return getRegex(mch);
		} else if(readerenv == null || !sexp) {
			throw message.getReadError("err.read.sharp.unknown", str);
			//throw new ReadException("unsupported #-syntax");
		} else {
			// pushResult(matched pattern);
			throw message.getReadError("err.read.sharp.unknown", str);
		}
	}*/

	//
	private Datum replaceRef(Datum d) {
		Stack<Datum> st = new Stack<Datum>();
		Stack<Datum> sr = new Stack<Datum>();
		Datum r;

		st.push(d);
		while(!st.isEmpty()) {
			r = st.pop();
			if(r instanceof Cons) {
				st.push(_STK_CONS);
				st.push(((Cons)r).getCar());
				st.push(((Cons)r).getCdr());
			} else if(r instanceof LispVector) {
				st.push(LispInteger.valueOf(((LispVector)r).size()));
				st.push(_STK_VEC);
				for(int i = 0; i < ((LispVector)r).size(); i++) {
					st.push(((LispVector)r).get(i));
				}
			} else if(r instanceof DumMarker) {
				Datum e = definedDatum.get(((DumMarker)r).number);

				if(nowNumber.contains(((DumMarker)r).number)) {
					sr.push(d);
				} else if(e instanceof DumMarker) {
					throw message.getReadError(
							"err.read.srfi38.invalidref");
				} else {
					sr.push(e);
				}
			} else if(r == _STK_CONS) {
				Datum xa = sr.pop();
				Datum xd = sr.pop();

				sr.push(new Cons(xa, xd));
			} else if(r == _STK_VEC) {
				LispVector v = new LispVector();
				int z = st.pop().getInt();

				for(int i = 0; i < z; i++) {
					v.add(sr.pop());
				}
				sr.push(v);
			} else {
				sr.push(r);
			}
		}
		return sr.pop();
	}

	//
	private static void replaceCirculate(Datum s, Datum d) {
		Stack<Datum> st = new Stack<Datum>();
		Datum r;

		st.push(s);
		while(!st.isEmpty()) {
			r = st.pop();
			if(r instanceof Cons) {
				Cons c = (Cons)r;

				if(c.getCar() == d) {
					c.setCar(s);
				} else {
					st.push(c.getCar());
				}

				if(c.getCdr() == d) {
					c.setCdr(s);
				} else {
					st.push(c.getCdr());
				}
			} else if(r instanceof LispVector) {
				LispVector v = (LispVector)r;

				for(int i = 0; i < ((LispVector)r).size(); i++) {
					if(v.get(i) == d) {
						v.setS(i, s);
					}
				}
			}
		}
	}

	//
	private void pushResult(Datum d) {
		resultStack.push(d);
	}

	//
	private static final String NUM_PS =
		"((-?([0-9]+|[0-9]*\\.[0-9]+)([esfdlESFDL]-?[0-9]+)?)|" +
		"(-?[0-9]+\\/[0-9]+))";
	private static final Pattern PTN =
		Pattern.compile(NUM_PS + "(.*)");
	private static final Pattern PTN_PRB =
		Pattern.compile(NUM_PS + "%");
	private static final Pattern PTN_ERR1 =
		Pattern.compile(NUM_PS + "\\+-" + NUM_PS);
	private static final Pattern PTN_ERR2 =
		Pattern.compile(NUM_PS + "\\+-" + NUM_PS + "%");
	private static final LispReal ONE_HUNDRED =
		LispInteger.valueOf(100);

	//
	private void pushMaybeSymbol(Datum sym) {
		Matcher m;
		String[] sp;
		String s1, s2, sn;
		LispNumber n, n2;
		LispReal r, r2;
		Quantity q;

		if(!(sym instanceof Symbol)) {
			// do nothing
		} else if((sn = ((Symbol)sym).getName()).indexOf(':') >= 0) {
			if((sp = sn.split(":")).length == 2) {
				n  = LispNumber.parse(sp[0], 10);
				n2 = LispNumber.parse(sp[1], 10);
				if(n != null && n2 != null &&
						n.isReal() && n2.isReal()) {
					pushResult(new LispRatio(
							n.getReal(), n2.getReal()));
					return;
				}
			}
		} else if((m = PTN_PRB.matcher(sn)).matches()) {
			s1 = m.group(1);
			r  = LispNumber.parse(s1, 10).getReal();
			r  = r.divide(ONE_HUNDRED);
			if(r.compareTo(LispInteger.ZERO) < 0 ||
					r.compareTo(LispInteger.ONE) > 0) {
				throw message.getError(
						"err.probability.range.invalid", r);
			}
			pushResult(new LispProbability(r));
			return;
		} else if((m = PTN_ERR1.matcher(sn)).matches()) {
			s1 = m.group(1);
			s2 = m.group(6);
			r  = LispNumber.parse(s1, 10).getReal();
			r2 = LispNumber.parse(s2, 10).getReal();
			pushResult(new LispRangedDouble(r.doubleValue(),
					r2.doubleValue()));
			return;
		} else if((m = PTN_ERR2.matcher(sn)).matches()) {
			s1 = m.group(1);
			s2 = m.group(6);
			r  = LispNumber.parse(s1, 10).getReal();
			r2 = LispNumber.parse(s2, 10).getReal();
			pushResult(new LispRangedDouble(r.doubleValue(),
					r.doubleValue() * r2.doubleValue() / 100.0));
			return;
		} else if(!LispUnitSystem.isUnitValid()) {
			// do nothing
		} else if((m = PTN.matcher(sn)).matches()) {
			s1 = m.group(1);
			s2 = m.group(6);
			n  = LispNumber.parse(s1, 10);
			if(n != null && !s2.equals("") && (q = Units.parse(
					LispUnitSystem.getDefault(), s2)) != null) {
				pushResult(new LispQuantity(
						q.getValue() * n.getRealDouble(),
						q.getUnit()));
				return;
			}
		}
		pushResult(sym);
	}

	//
	private Datum putDefinedDatum(Datum d) {
		Datum e;

		if(d instanceof DumMarker) {
			throw message.getReadError("err.read.srfi38.invalidref");
		}

		e = replaceRef(d);
		replaceCirculate(e, d);
		for(Object num : nowNumber) {
			definedDatum.put(num, e);
		}
		nowNumber = new HashSet<Object>();
		return e;
	}

	//
	private void delParenthesis() {
		nowNumber = numberStack.pop();
	}

	//
	private void addNowNumber(Object num) {
		numberStack.push(nowNumber);
		if(!nowNumber.isEmpty()) {
			nowNumber = new HashSet<Object>();
		}
		definedDatum.put(num, new DumMarker(num));
		nowNumber.add(num);
	}

	//
	private int getByCharCode(String str) {
		int c;

		try {
			if((c = Integer.parseInt(str, 16)) < 0 ||
					c > Character.MAX_CODE_POINT) {
				throw message.getReadError("err.read.character.code",
						str);
			} else if(Character.isHighSurrogate((char)c) ||
					Character.isLowSurrogate((char)c)) {
				throw message.getReadError("err.read.character.code",
						str);
			} else {
				return c;
			}
		} catch(NumberFormatException e) {
			throw message.getReadError("err.read.character.code",
					str);
		}
	}

	//
	private LispCharacter getCharacter() {
		String str = getToken();

		clearBuf();
		if(str.length() <= 0) {
			throw message.getReadError("err.read.character.unknown",
					str);
		} else if(str.length() == 1) {
			return new LispCharacter(str.charAt(0));
		} else if(str.charAt(0) == 'u' ||
				str.charAt(0) == 'x' ||
				str.charAt(0) == 'U' ||
				str.charAt(0) == 'X') {
			return LispCharacter.valueOf(
					getByCharCode(str.substring(1)));
		} else {
			LispCharacter r = LispCharacter.getByName(str);

			if(r == null) {
				throw message.getReadError(
						"err.read.character.unknown", str);
				//throw new ReadException(
				//		"unknown character name:" + str);
			} else {
				return r;
			}
		}
	}

	//
	private LispString getString() {
		String str = getToken();

		clearBuf();
		return new LispString(str);
	}

	//
	private Datum getSymbol() {
		String str = getToken() + buf3.toString();

		clearBuf();
		if("+nan.0".equals(str)) {
			return LispDouble.NaN;
		} else if(str.length() >= 2 && str.charAt(0) == ':') {
			return Keyword.getKeyword(str.substring(1));
		} else if(str.length() >= 2 &&
				str.charAt(str.length() - 1) == ':') {
			return Keyword.getKeyword(str.substring(
					0, str.length() - 1));
		}
		return Symbol.getSymbol(str);
	}

	//
	private Symbol getQuote() {
		String str = getToken();

		clearBuf();
		if("\'".equals(str)) {
			return Symbol.QUOTE;
		} else if("`".equals(str)) {
			return Symbol.QUASIQUOTE;
		} else if(",".equals(str)) {
			return Symbol.UNQUOTE;
		} else if(",@".equals(str)) {
			return Symbol.UNQUOTE_SPLICING;
		} else {
			throw new RuntimeException("unknown quote:" + str);
		}
	}

	//
	private State popStateStack() {
		while(true) {
			State poped;

			if(stateStack.empty()) {
				return State.INIT;
			}

			poped = stateStack.pop();
			if(State.QUOTE.equals(poped)) {
				Datum c31 = resultStack.pop();
				Cons  c32 = consStack.pop();

				if(c31 == _VOID) {
					stateStack.push(poped);
					consStack.push(c32);
					return State.INIT;
				} else {
					c32.setCar(c31);
				}
			} else if(State.S_EXP_COMMENT.equals(poped)) {
				Datum dd = resultStack.pop();

				if(_DOT.equals(dd)) {
					throw message.getReadError("err.read.dot");
					//throw new ReadException("bad dot syntax");
				}
				pushResult(_VOID);
			} else if(State.S_EXP_MACRO.equals(poped)) {
				Datum dd = resultStack.pop();  // pop s-exp
				Datum mt = resultStack.pop();  // pop matched re
				Datum sy = resultStack.pop();
//				Cons re = new Cons(sy, new Cons(mt,
//						new Cons(dd, Nil.NIL)));
				Datum aa = LispUtils.list(sy, mt,
						LispUtils.list(Symbol.getSymbol("quote"), dd));

				while(stateStack.peek().equals(State.QUOTE)) {
					Datum c31 = resultStack.pop();
					Cons  c32 = consStack.pop();

					stateStack.pop();
					if(c31 == _VOID) {
						stateStack.push(poped);
						consStack.push(c32);
						return State.INIT;
					}
				}
				// expand read macro
				pushResult(aa);
			} else if(State.S_EXP_QUOTE.equals(poped)) {
				Datum dd = resultStack.pop();  // pop s-exp
				Datum sy = resultStack.pop();
				Cons re = new Cons(sy, new Cons(dd, Nil.NIL));

				// expand read macro
				pushResult(re);
			} else if(State.SHARP_SYNTAX_SRFI38_2.equals(poped)) {
				delParenthesis();
				resultStack.push(putDefinedDatum(resultStack.pop()));
			} else {
				return poped;
			}
		}
	}

	//
	private void checkParenthesis(int r) {
		int p = parenStack.pop();

		if(p == '(' && r == ')') {
			// ok
		} else if(p == '[' && r == ']') {
			// ok
		} else if(p == '{' && r == '}') {
			// ok
		} else if(p == '<' && r == '>') {
			// ok
		} else {
			throw message.getReadError(
					"err.read.parenthesis.mismatch");
		}
	}

	//
	private boolean parenthesis(int r) {
		int p = parenStack.isEmpty() ? -1 : parenStack.peek();

		return (r == ')' || r == ']' ||
				(p == '{' && r == '}') ||
				(p == '<' && r == '>'));
	}

	//
	private Datum getRange(int r, Datum p1) {
		parenStack.pop();
		if(p1 instanceof LispReal) {
			return new LispRealNumberSet(
					Intervals.newClosedInterval(p1, p1));
		} else if(p1 instanceof LispCharacter) {
			return new LispCharSet(p1.getCharacterCodePoint());
		} else {
			throw message.getError("err.read.range.type");
		}
	}

	//
	private Datum getRange(int r, Datum p1, Datum p2) {
		int s = parenStack.pop();

		if(p1 instanceof LispReal && p2 instanceof LispReal) {
			LispReal r1 = (LispReal)p1, r2 = (LispReal)p2;
			int a = ((LispReal)p1).compareTo((LispReal)p2);
			boolean o1, o2;
			Interval i1;

			if(s == '[' && r == ']') {
				if(a > 0) {
					throw message.getError("err.read.range.type");
				}
			} else if(s == '(' && r == ')') {
				if(a >= 0) {
					throw message.getError("err.read.range.type");
				}
			} else if(s == '(' && r == ']') {
				if(a >= 0) {
					throw message.getError("err.read.range.type");
				}
			} else if(s == '[' && r == ')') {
				if(a >= 0) {
					throw message.getError("err.read.range.type");
				}
			} else {
				throw message.getError("err.read.range.type");
			}

			o1 = s == '(';
			o2 = r == ')';
			if(r1.isNaN() || r2.isNaN()) {
				return LispRealNumberSet.O;
			} else if(!r1.isInfinity() && !r2.isInfinity()) {
				i1 = Interval.newInstance(r1, o1, r2, o2);
			} else if(r1.isInfinity() && !r2.isInfinity()) {
				if(r1.signum() >= 0) {
					return LispRealNumberSet.O;
				} else {
					i1 = Intervals.newInfimumlessInterval(r2, o2);
				}
			} else if(!r1.isInfinity() && r2.isInfinity()) {
				if(r2.signum() <= 0) {
					return LispRealNumberSet.O;
				} else {
					i1 = Intervals.newSupremumlessInterval(r1, o1);
				}
			} else {
				if(r1.signum() >= 0 || r2.signum() <= 0) {
					return LispRealNumberSet.O;
				} else {
					return LispRealNumberSet.U;
				}
			}
			return new LispRealNumberSet(i1);
		} else if(p1 instanceof LispCharacter &&
				p2 instanceof LispCharacter) {
			int a = ((LispCharacter)p1).compareTo((LispCharacter)p2);

			if(s == '[' && r == ']') {
				if(a > 0) {
					throw message.getError("err.read.range.type");
				} else {
					return LispCharSet.getInterval(
							p1.getCharacterCodePoint(),
							p2.getCharacterCodePoint());
				}
			} else {
				throw message.getError("err.read.range.type");
			}
		} else {
			throw message.getError("err.read.range.type");
		}
	}

	//
	private void processHashBang(String s) {
		if(s.equals("r6rs")) {
			message.warn("warn.r6rs.notsupport");
		} else if(s.equals("schluessel")) {
			// do nothing
		} else if(s.equals("no-fold-case")) {
			Symbol.foldCase = Symbol.NO_FOLD_CASE;
		} else if(s.equals("fold-case")) {
			Symbol.foldCase = Symbol.LOWER_FOLD_CASE;
		} else if(s.equals("upper-fold-case")) {
			Symbol.foldCase = Symbol.UPPER_FOLD_CASE;
		} else if(s.equals("lower-fold-case")) {
			Symbol.foldCase = Symbol.LOWER_FOLD_CASE;
		} else {
			message.warn("warn.unknown.hashbang");
		}
	}

	/**
	 * 
	 * @param exp
	 * @return
	 * @throws IOException
	 */
	public boolean readParse(String exp) throws IOException {
		read(exp);
		return parse();
	}

	/**
	 * 
	 * @param read
	 * @throws IOException 
	 */
	public void read(String read) throws IOException {
		if(rd != null) {
			rd.close();
		}
		rd = new InputPort(new StringReader(read), message);
	}

	/**
	 * 
	 * @return
	 */
	public Datum getDatum() {
		while(!resultStack.empty() && resultStack.get(0) == _VOID) {
			resultStack.remove(0);
		}

		if(stateStack.empty() && !resultStack.empty()) {
			return resultStack.get(0);
		} else {
			return null;
		}
	}

	/**
	 * 
	 * @return
	 */
	public List<Datum> getData() {
		return Collections.unmodifiableList(resultStack);
	}

	/**
	 * 
	 * @return
	 */
	public boolean isReadBefore() {
		return readBefore;
	}

	//
	private boolean isStackOK() {
		return (stateStack.empty() || (stateStack.size() == 1 &&
				stateStack.peek().equals(State.INIT)));
	}

	/**
	 * 
	 */
	public void clear() {
		resultStack.clear();
		stateStack.clear();
		consStack.clear();

		clearBuf();
		stateStack.push(State.INIT);
		state = State.INIT;
		readBefore = false;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isEOF() {
		return state != null && state.equals(State.END_OF_FILE);
	}

	/**
	 * 
	 * @return
	 * @throws IOException
	 */
	public boolean parse() throws IOException {
		int r, r0, imgn, reg0 = 0;

		if(rd == null) {
			throw new IllegalStateException();
		}

		if(stateStack.empty() && State.INIT.equals(state)) {
			stateStack.push(State.INIT);
		}

		if(State.STRING.equals(state) ||
				State.STRING_NEWLINE.equals(state) ||
				State.STRING_NEWLINE2.equals(state)) {
			r = read();
		} else {
			readBefore = false;
			r = readNext();
		}

		ParserSharpSyntax.Engine eng = null;
		while(true) {
			if(!consoleMode && readBefore && stateStack.empty()) {
				if(r >= 0) {
					rd.unread((char)r);
				}
				return true;
			}

			switch(state) {
			case INIT:
				if(r == '(' || r == '[') {
					parenStack.push(r);
					state = State.LIST;
					r = readNext();
				} else if(parenthesis(r)) {
					throw message.getReadError(
							"err.read.closeparenthesis");
					//throw new ReadException("extra close parenthesis");
				} else if(r == '\'' || r == '`' || r == ',') {
					appendBuf(r);
					state = State.QUOTE;
					r = readNext2();
				} else if(r == '-' || r == '+') {
					appendBuf(r);
					state = State.HEAD_SIGN;
					r = read();
				} else if(r == '\"') {
					state = State.STRING;
					r = read();
				} else if(isDigit09(r)) {
					appendBuf(r);
					state = State.NUMBER;
					r = read();
				} else if(r == '#') {
					state = State.SHARP_SYNTAX_I;
					r = read();
				} else if(r == EOF) {
					//return stateStack.empty();
					state = State.END_OF_FILE;
					return isStackOK();
				} else if(r == '.') {
					appendBuf(r);
					state = State.FLOAT_NUMBER;
					r = read();
				} else if(r == ';') {
					stateStack.push(state);
					state = State.COMMENT;
					r = read();
				} else if(r == '|') {
					state = State.SYMBOLQUOTE;
					r = read();
				} else {
					appendBuf(r);
					state = State.SYMBOL;
					r = read();
				}
				break;
			case NUMBER_PREFIX:
				if(r == EOF) {
					throw message.getReadError("err.read.sharp.unknown");
					//throw new ReadException("bad #-syntax");
				} else if(r == '#') {
					r = read();

					if(r == 'e' || r == 'E') {
						if(!Exact.DEFAULT.equals(exactness)) {
							throw message.getReadError(
									"err.read.numberprefix");
						}
						exactness = Exact.EXACT;
						state = State.NUMBER_PREFIX;
					} else if(r == 'i' || r == 'I') {
						if(!Exact.DEFAULT.equals(exactness)) {
							throw message.getReadError(
									"err.read.numberprefix");
						}
						exactness = Exact.INEXACT;
						state = State.NUMBER_PREFIX;
					} else if(r == 'b' || r == 'B') {
						if(radix > 0) {
							throw message.getReadError(
									"err.read.numberprefix");
						}
						radix = 2;
						state = State.NUMBER_PREFIX;
					} else if(r == 'o' || r == 'O') {
						if(radix > 0) {
							throw message.getReadError(
									"err.read.numberprefix");
						}
						radix = 8;
						state = State.NUMBER_PREFIX;
					} else if(r == 'd' || r == 'D') {
						if(radix > 0) {
							throw message.getReadError(
									"err.read.numberprefix");
						}
						radix = 10;
						state = State.NUMBER_PREFIX;
					} else if(r == 'x' || r == 'X') {
						if(radix > 0) {
							throw message.getReadError(
									"err.read.numberprefix");
						}
						radix = 16;
						state = State.NUMBER_PREFIX;
					}
					r = read();
				} else if(r == '-' || r == '+') {
					appendBuf(r);
					state = State.HEAD_SIGN;
					r = read();
				} else if(isDigit09az(r)) {
					appendBuf(r);
					state = State.NUMBER;
					r = read();
				} else if(r == '.') {
					appendBuf(r);
					state = State.FLOAT_NUMBER;
					r = read();
				} else if(isImaginary(r)) {
					appendBuf(r);
					state = State.COMPLEX_I;
					r = read();
				} else {
					throw message.getReadError(
							"err.read.numberprefix");
					//throw new ReadException("bad numeric format");
				}
				break;
			case HEAD_SIGN:
				signed = true;

				if(isDigit09az(r)) {
					state = State.NUMBER;
				} else if(r == '.') {
					appendBuf(r);
					state = State.FLOAT_NUMBER;
					r = read();
				} else if(r == 'i') {
					appendBuf(r);
					//state = State.COMPLEX_I;
					state = State.INF_I;
					r = read();
				} else if(isImaginary(r)) {
					appendBuf(r);
					state = State.COMPLEX_I;
					r = read();
				} else {
					state = symbolNum();
				}
				break;
			case NUMBER:
				if(isDigit09az(r)) {
					appendBuf(r);
					r = read();
				} else if(r == '.') {
					appendBuf(r);
					state = State.FLOAT_NUMBER;
					r = read();
				} else if(isExpsym(r)) {
					state = State.FLOAT_NUMBER_E;
					if(r == 'd') {
						r = read();
						if(r == 'f' || r == 'F') {
							appendBuf('#');
							r = read();
						} else if(r == 'd' || r == 'D') {
							appendBuf('&');
							r = read();
						} else {
							appendBuf('d');
						}
					} else {
						appendBuf(Character.toLowerCase(r));
						r = read();
					}
				} else if(r == '(' || r == '[' || r == '[') {
					pushResult(getInteger());
					state = popStateStack();
				} else if(parenthesis(r)) {
					pushResult(getInteger());
					state = popStateStack();
				} else if(Character.isWhitespace(r)) {
					pushResult(getInteger());
					state = popStateStack();
					r = readNext();
				} else if(r == ';') {
					pushResult(getInteger());
					state = popStateStack();
				} else if(r == EOF) {
					pushResult(getInteger());
					state = popStateStack();
					return stateStack.empty();
				} else if(r == '/') {
					appendBuf(r);
					state = State.RATIONAL_NUMBER;
					r = read();
				} else if(r == '+' || r == '-') {
					appendBuf2(r);
					state = State.HEAD_SIGN_COMPLEX;
					r = read();
				} else if(isImaginary(r)) {
					appendBuf(r);
					state = State.COMPLEX_I;
					r = read();
				} else if(r == '@') {
					appendBuf(r);
					state = State.INIT_POLAR;
					r = read();
				} else {
					state = symbolNum();
				}
				break;
			case FLOAT_NUMBER:
				if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					state = popStateSymbolNum();
				} else if(parenthesis(r)) {
					state = popStateSymbolNum();
				} else if(r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(isDigit09az(r)) {
					state = State.FLOAT_NUMBER_D;
				} else if(r == '{') {
					appendBuf(r);
					r = read();
					state = State.FLOAT_NUMBER_CYCLE;
				} else {
					state = symbolNum();
				}
				break;
			case FLOAT_NUMBER_D:
				if(r == EOF) {
					pushResult(getDouble());
					state = popStateStack();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					pushResult(getDouble());
					state = popStateStack();
				} else if(parenthesis(r)) {
					pushResult(getDouble());
					state = popStateStack();
				} else if(r == ';') {
					pushResult(getDouble());
					state = popStateStack();
				} else if(Character.isWhitespace(r)) {
					pushResult(getDouble());
					state = popStateStack();
					r = readNext();
				} else if(isDigit09az(r)) {
					appendBuf(r);
					r = read();
				} else if(isExpsym(r)) {
					state = State.FLOAT_NUMBER_E;
					if(r == 'd') {
						r = read();
						if(r == 'f' || r == 'F') {
							appendBuf('#');
							r = read();
						} else if(r == 'd' || r == 'D') {
							appendBuf('&');
							r = read();
						} else {
							appendBuf('d');
						}
					} else {
						appendBuf(Character.toLowerCase(r));
						r = read();
					}
				} else if(r == '+' || r == '-') {
					appendBuf2(r);
					state = State.HEAD_SIGN_COMPLEX;
					r = read();
				} else if(isImaginary(r)) {
					appendBuf(r);
					state = State.COMPLEX_I;
					r = read();
				} else if(r == '@') {
					appendBuf(r);
					state = State.INIT_POLAR;
					r = read();
				} else if(r == '{') {
					appendBuf(r);
					r = read();
					state = State.FLOAT_NUMBER_CYCLE;
				} else if(r == '.') {
					appendBuf(r);
					r = read();
					state = State.FLOAT_NUMBER_ELLIPSE1;
				} else {
					state = symbolNum();
				}
				break;
			case FLOAT_NUMBER_CYCLE:
				if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '(' || r == '[' || parenthesis(r) ||
						r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(isDigit09az(r)) {
					appendBuf(r);
					r = read();
				} else if(r == '}') {
					appendBuf(r);
					pushResult(getDouble());
					state = popStateStack();
					r = readNext();
				} else {
					state = symbolNum();
				}
				break;
			case FLOAT_NUMBER_ELLIPSE1:
				if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '(' || r == '[' || parenthesis(r) ||
						r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(r == '.') {
					r = read();
					state = State.FLOAT_NUMBER_ELLIPSE2;
				} else {
					state = symbolNum();
				}
				break;
			case FLOAT_NUMBER_ELLIPSE2:
				if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '(' || r == '[' || parenthesis(r) ||
						r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(r == '.') {
					pushResult(getDouble());
					state = popStateStack();
					r = readNext();
				} else {
					state = symbolNum();
				}
				break;
			case FLOAT_NUMBER_E:
				if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					state = popStateSymbolNum();
				} else if(parenthesis(r)) {
					state = popStateSymbolNum();
				} else if(r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(r == '+' || r == '-') {
					appendBuf(r);
					state = State.FLOAT_NUMBER_E_SIGN;
					r = read();
				} else if(isDigit09az(r)) {
					state = State.FLOAT_NUMBER_E_D;
				} else {
					state = symbolNum();
				}
				break;
			case FLOAT_NUMBER_E_SIGN:
				if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					state = popStateSymbolNum();
				} else if(parenthesis(r)) {
					state = popStateSymbolNum();
				} else if(r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(isDigit09az(r)) {
					state = State.FLOAT_NUMBER_E_D;
				} else {
					state = symbolNum();
				}
				break;
			case FLOAT_NUMBER_E_D:
				if(r == EOF) {
					pushResult(getDouble());
					state = popStateStack();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					pushResult(getDouble());
					state = popStateStack();
				} else if(parenthesis(r)) {
					pushResult(getDouble());
					state = popStateStack();
				} else if(r == ';') {
					pushResult(getDouble());
					state = popStateStack();
				} else if(Character.isWhitespace(r)) {
					pushResult(getDouble());
					state = popStateStack();
					r = readNext();
				} else if(isDigit09az(r)) {
					appendBuf(r);
					r = read();
				} else if(r == '+' || r == '-') {
					appendBuf2(r);
					state = State.HEAD_SIGN_COMPLEX;
					r = read();
				} else if(isImaginary(r)) {
					appendBuf(r);
					state = State.COMPLEX_I;
					r = read();
				} else if(r == '@') {
					appendBuf(r);
					state = State.INIT_POLAR;
					r = read();
				} else {
					state = symbolNum();
				}
				break;
			case RATIONAL_NUMBER:
				if(isDigit09az(r)) {
					appendBuf(r);
					r = read();
					state = State.RATIONAL_NUMBER_2;
				} else if(r == '(' || r == '[') {
					state = popStateSymbolNum();
				} else if(parenthesis(r)) {
					state = popStateSymbolNum();
				} else if(r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else {
					state = symbolNum();
				}
				break;
			case RATIONAL_NUMBER_2:
				if(isDigit09az(r)) {
					appendBuf(r);
					r = read();
				} else if(r == '(' || r == '[') {
					pushResult(getRational());
					state = popStateStack();
				} else if(parenthesis(r)) {
					pushResult(getRational());
					state = popStateStack();
				} else if(r == ';') {
					pushResult(getRational());
					state = popStateStack();
				} else if(Character.isWhitespace(r)) {
					pushResult(getRational());
					state = popStateStack();
					r = readNext();
				} else if(r == EOF) {
					pushResult(getRational());
					state = popStateStack();
					return stateStack.empty();
				} else if(r == '+' || r == '-') {
//					appendBuf2(r);
//					state = State.HEAD_SIGN_COMPLEX;
					reg0 = r;
					state = State.CONTINUED_FRACTION;
					r = read();
				} else if(isImaginary(r)) {
					appendBuf(r);
					state = State.COMPLEX_I;
					r = read();
				} else if(r == '@') {
					appendBuf(r);
					state = State.INIT_POLAR;
					r = read();
				} else {
					state = symbolNum();
				}
				break;
			case CONTINUED_FRACTION:
				if(r == '/') {
					appendBuf(reg0);  appendBuf(r);
					state = State.RATIONAL_NUMBER;
					r = read();
				} else {
					appendBuf2(reg0);
					state = State.HEAD_SIGN_COMPLEX;
				}
				break;
			case INIT_POLAR:
				if(r == '-' || r == '+') {
					appendBuf(r);
					state = State.HEAD_SIGN_POLAR;
					r = read();
				} else if(isDigit09az(r)) {
					appendBuf(r);
					state = State.NUMBER_POLAR;
					r = read();
				} else if(r == '.') {
					appendBuf(r);
					state = State.FLOAT_NUMBER_POLAR;
					r = read();
				} else {
					state = symbolNum();
				}
				break;
			case HEAD_SIGN_POLAR:
				if(isDigit09az(r)) {
					state = State.NUMBER_POLAR;
				} else if(r == '.') {
					appendBuf(r);
					state = State.FLOAT_NUMBER_POLAR;
					r = read();
				} else {
					state = symbolNum();
				}
				break;
			case NUMBER_POLAR:
				if(isDigit09az(r)) {
					appendBuf(r);
					r = read();
				} else if(r == '.') {
					appendBuf(r);
					state = State.FLOAT_NUMBER_POLAR;
					r = read();
				} else if(isExpsym(r)) {
					state = State.FLOAT_NUMBER_E_POLAR;
					if(r == 'd') {
						r = read();
						if(r == 'f' || r == 'F') {
							appendBuf('#');
							r = read();
						} else if(r == 'd' || r == 'D') {
							appendBuf('&');
							r = read();
						} else {
							appendBuf('d');
						}
					} else {
						appendBuf(Character.toLowerCase(r));
						r = read();
					}
				} else if(r == '(' || r == '[') {
					pushResult(getPolar());
					state = popStateStack();
				} else if(parenthesis(r)) {
					pushResult(getPolar());
					state = popStateStack();
				} else if(r == ';') {
					pushResult(getPolar());
					state = popStateStack();
				} else if(Character.isWhitespace(r)) {
					pushResult(getPolar());
					state = popStateStack();
					r = readNext();
				} else if(r == EOF) {
					pushResult(getPolar());
					state = popStateStack();
					return stateStack.empty();
				} else if(r == '/') {
					appendBuf(r);
					state = State.RATIONAL_NUMBER_POLAR;
					r = read();
				} else {
					state = symbolNum();
				}
				break;
			case FLOAT_NUMBER_POLAR:
				if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					state = popStateSymbolNum();
				} else if(parenthesis(r)) {
					state = popStateSymbolNum();
				} else if(r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(isDigit09az(r)) {
					state = State.FLOAT_NUMBER_D_POLAR;
				} else {
					state = symbolNum();
				}
				break;
			case FLOAT_NUMBER_D_POLAR:
				if(r == EOF) {
					pushResult(getPolar());
					state = popStateStack();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					pushResult(getPolar());
					state = popStateStack();
				} else if(parenthesis(r)) {
					pushResult(getPolar());
					state = popStateStack();
				} else if(r == ';') {
					pushResult(getPolar());
					state = popStateStack();
				} else if(Character.isWhitespace(r)) {
					pushResult(getPolar());
					state = popStateStack();
					r = readNext();
				} else if(isDigit09az(r)) {
					appendBuf(r);
					r = read();
				} else if(isExpsym(r)) {
					state = State.FLOAT_NUMBER_E_POLAR;
					if(r == 'd') {
						r = read();
						if(r == 'f' || r == 'F') {
							appendBuf('#');
							r = read();
						} else if(r == 'd' || r == 'D') {
							appendBuf('&');
							r = read();
						} else {
							appendBuf('d');
						}
					} else {
						appendBuf(Character.toLowerCase(r));
						r = read();
					}
				} else {
					state = symbolNum();
				}
				break;
			case FLOAT_NUMBER_E_POLAR:
				if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					state = popStateSymbolNum();
				} else if(parenthesis(r)) {
					state = popStateSymbolNum();
				} else if(r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(r == '+' || r == '-') {
					appendBuf(r);
					state = State.FLOAT_NUMBER_E_SIGN_POLAR;
					r = read();
				} else if(isDigit09az(r)) {
					state = State.FLOAT_NUMBER_E_D_POLAR;
				} else {
					state = symbolNum();
				}
				break;
			case FLOAT_NUMBER_E_SIGN_POLAR:
				if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					state = popStateSymbolNum();
				} else if(parenthesis(r)) {
					state = popStateSymbolNum();
				} else if(r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(isDigit09az(r)) {
					state = State.FLOAT_NUMBER_E_D_POLAR;
				} else {
					state = symbolNum();
				}
				break;
			case FLOAT_NUMBER_E_D_POLAR:
				if(r == EOF) {
					pushResult(getPolar());
					state = popStateStack();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					pushResult(getPolar());
					state = popStateStack();
				} else if(parenthesis(r)) {
					pushResult(getPolar());
					state = popStateStack();
				} else if(r == ';') {
					pushResult(getPolar());
					state = popStateStack();
				} else if(Character.isWhitespace(r)) {
					pushResult(getPolar());
					state = popStateStack();
					r = readNext();
				} else if(isDigit09az(r)) {
					appendBuf(r);
					r = read();
				} else {
					state = symbolNum();
				}
				break;
			case RATIONAL_NUMBER_POLAR:
				if(isDigit09az(r)) {
					appendBuf(r);
					r = read();
					state = State.RATIONAL_NUMBER_2_POLAR;
				} else if(r == '(' || r == '[') {
					state = popStateSymbolNum();
				} else if(parenthesis(r)) {
					state = popStateSymbolNum();
				} else if(r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else {
					state = symbolNum();
				}
				break;
			case RATIONAL_NUMBER_2_POLAR:
				if(isDigit09az(r)) {
					appendBuf(r);
					r = read();
				} else if(r == '(' || r == '[') {
					pushResult(getPolar());
					state = popStateStack();
				} else if(parenthesis(r)) {
					pushResult(getPolar());
					state = popStateStack();
				} else if(r == ';') {
					pushResult(getPolar());
					state = popStateStack();
				} else if(Character.isWhitespace(r)) {
					pushResult(getPolar());
					state = popStateStack();
					r = readNext();
				} else if(r == EOF) {
					pushResult(getPolar());
					state = popStateStack();
					return stateStack.empty();
				} else {
					state = symbolNum();
				}
				break;
			case INF_I:
				imgn = I_BIT;
				if(r == 'o') {
					imgn = imgn | L_BIT;
					r = read();
				}

				if(r == EOF) {
					pushResult(getImaginary(imgn));
					state = popStateStack();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					pushResult(getImaginary(imgn));
					state = popStateStack();
				} else if(parenthesis(r)) {
					pushResult(getImaginary(imgn));
					state = popStateStack();
				} else if(r == ';') {
					pushResult(getImaginary(imgn));
					state = popStateStack();
				} else if(Character.isWhitespace(r)) {
					pushResult(getImaginary(imgn));
					state = popStateStack();
					r = readNext();
				} else if(r == 'n') {
					appendBuf(r);
					state = State.INF_N;
					r = read();
				} else if(r == '+' || r == '-') {
					bufi[I_BIT] = buf;
					buf = new StringBuilder("0");
					appendBuf2(r);
					state = State.HEAD_SIGN_COMPLEX;
					r = read();
				} else {
					state = symbolNum();
				}
				break;
			case INF_N:
				if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					state = popStateSymbolNum();
				} else if(parenthesis(r)) {
					state = popStateSymbolNum();
				} else if(r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(r == 'f') {
					appendBuf(r);
					state = State.INF_F;
					r = read();
				} else {
					state = symbolNum();
				}
				break;
			case INF_F:
				if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					state = popStateSymbolNum();
				} else if(parenthesis(r)) {
					state = popStateSymbolNum();
				} else if(r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(r == '.') {
					appendBuf(r);
					state = State.INF_DOT;
					r = read();
				} else {
					state = symbolNum();
				}
				break;
			case INF_DOT:
				if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					state = popStateSymbolNum();
				} else if(parenthesis(r)) {
					state = popStateSymbolNum();
				} else if(r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(r == '0') {
					appendBuf(r);
					state = State.INF_0;
					r = read();
				} else {
					state = symbolNum();
				}
				break;
			case INF_0:
				if(r == EOF) {
					pushResult(getDouble());
					state = popStateStack();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					pushResult(getDouble());
					state = popStateStack();
				} else if(parenthesis(r)) {
					pushResult(getDouble());
					state = popStateStack();
				} else if(r == ';') {
					pushResult(getDouble());
					state = popStateStack();
				} else if(Character.isWhitespace(r)) {
					pushResult(getDouble());
					state = popStateStack();
					r = readNext();
				} else if(r == '+' || r == '-') {
					appendBuf2(r);
					state = State.HEAD_SIGN_COMPLEX;
					r = read();
				} else if(isImaginary(r)) {
					appendBuf(r);
					state = State.COMPLEX_I;
					r = read();
				} else if(r == '@') {
					appendBuf(r);
					state = State.INIT_POLAR;
					r = read();
				} else {
					state = symbolNum();
				}
				break;
			case COMPLEX_I:
				r0   = buf.charAt(buf.length() - 1);
				imgn = getImaginaryNo(r0);
				if(r == 'o') {
					if(r0 == 'o') {
						state = symbolNum();
						break;
					} else {
						imgn = imgn | L_BIT;
						r = read();
					}
				}
				bufi[imgn] = buf;

				if(r == '(' || r == '[') {
					if(signed) {
						pushResult(getImaginary(imgn));
						state = popStateStack();
					} else {
						state = popStateSymbolNum();
					}
				} else if(parenthesis(r)) {
					if(signed) {
						pushResult(getImaginary(imgn));
						state = popStateStack();
					} else {
						state = popStateSymbolNum();
					}
				} else if(r == ';') {
					if(signed) {
						pushResult(getImaginary(imgn));
						state = popStateStack();
					} else {
						state = popStateSymbolNum();
					}
				} else if(Character.isWhitespace(r)) {
					if(signed) {
						pushResult(getImaginary(imgn));
						state = popStateStack();
					} else {
						state = popStateSymbolNum();
					}
					r = readNext();
				} else if(r == '+' || r == '-') {
					buf = new StringBuilder("0");
					appendBuf2(r);
					state = State.HEAD_SIGN_COMPLEX;
					r = read();
				} else if(r == EOF) {
					if(signed) {
						pushResult(getImaginary(imgn));
						state = popStateStack();
					} else {
						state = popStateSymbolNum();
					}
					return stateStack.empty();
				} else {
					state = symbolNum();
				}
				break;
			case HEAD_SIGN_COMPLEX:
				if(isDigit09az(r)) {
					state = State.NUMBER_COMPLEX;
				} else if(r == '.') {
					appendBuf2(r);
					state = State.FLOAT_NUMBER_COMPLEX;
					r = read();
				} else if(r == 'i') {
					appendBuf2(r);
					state = State.INF_I_COMPLEX;
					r = read();
				} else if(isImaginary(r)) {
					appendBuf2(r);
					state = State.COMPLEX_I2;
					r = read();
				} else {
					state = symbol2Num();
				}
				break;
			case NUMBER_COMPLEX:
				if(isDigit09az(r)) {
					appendBuf2(r);
					r = read();
				} else if(r == '.') {
					appendBuf2(r);
					state = State.FLOAT_NUMBER_COMPLEX;
					r = read();
				} else if(isExpsym(r)) {
					state = State.FLOAT_NUMBER_E_COMPLEX;
					if(r == 'd') {
						r = read();
						if(r == 'f' || r == 'F') {
							appendBuf2('#');
							r = read();
						} else if(r == 'd' || r == 'D') {
							appendBuf2('&');
							r = read();
						} else {
							appendBuf2('d');
						}
					} else {
						appendBuf2(Character.toLowerCase(r));
						r = read();
					}
				} else if(r == '(' || r == '[') {
					state = popStateSymbolNum();
				} else if(parenthesis(r)) {
					state = popStateSymbolNum();
				} else if(r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '/') {
					appendBuf2(r);
					state = State.RATIONAL_NUMBER_COMPLEX;
					r = read();
				} else if(isImaginary(r)) {
					appendBuf2(r);
					state = State.COMPLEX_I2;
					r = read();
				} else {
					state = symbol2Num();
				}
				break;
			case FLOAT_NUMBER_COMPLEX:
				if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					state = popStateSymbolNum();
				} else if(parenthesis(r)) {
					state = popStateSymbolNum();
				} else if(r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(isDigit09az(r)) {
					state = State.FLOAT_NUMBER_D_COMPLEX;
				} else if(r == '{') {
					appendBuf2(r);
					r = read();
					state = State.FLOAT_NUMBER_COMPLEX_CYCLE;
				} else {
					state = symbol2Num();
				}
				break;
			case FLOAT_NUMBER_D_COMPLEX:
				if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					state = popStateSymbolNum();
				} else if(parenthesis(r)) {
					state = popStateSymbolNum();
				} else if(r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(isDigit09az(r)) {
					appendBuf2(r);
					r = read();
				} else if(isExpsym(r)) {
					state = State.FLOAT_NUMBER_E_COMPLEX;
					if(r == 'd') {
						r = read();
						if(r == 'f' || r == 'F') {
							appendBuf2('#');
							r = read();
						} else if(r == 'd' || r == 'D') {
							appendBuf2('&');
							r = read();
						} else {
							appendBuf2('d');
						}
					} else {
						appendBuf2(Character.toLowerCase(r));
						r = read();
					}
				} else if(isImaginary(r)) {
					appendBuf2(r);
					state = State.COMPLEX_I2;
					r = read();
				} else if(r == '{') {
					appendBuf2(r);
					r = read();
					state = State.FLOAT_NUMBER_COMPLEX_CYCLE;
				} else if(r == '.') {
					appendBuf2(r);
					r = read();
					state = State.FLOAT_NUMBER_COMPLEX_ELLIPSE1;
				} else {
					state = symbol2Num();
				}
				break;
			case FLOAT_NUMBER_COMPLEX_CYCLE:
				if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					state = popStateSymbolNum();
				} else if(parenthesis(r)) {
					state = popStateSymbolNum();
				} else if(r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(isDigit09az(r)) {
					appendBuf2(r);
					r = read();
				} else if(r == '}') {
					appendBuf2(r);
					r = read();
					state = State.FLOAT_NUMBER_COMPLEX_CYCLE2;
				} else {
					state = symbolNum();
				}
				break;
			case FLOAT_NUMBER_COMPLEX_CYCLE2:
				if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '(' || r == '[' || parenthesis(r) ||
						r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(isImaginary(r)) {
					appendBuf2(r);
					state = State.COMPLEX_I2;
					r = read();
				} else {
					state = symbolNum();
				}
				break;
			case FLOAT_NUMBER_COMPLEX_ELLIPSE1:
				if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '(' || r == '[' || parenthesis(r) ||
						r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(r == '.') {
					r = read();
					state = State.FLOAT_NUMBER_COMPLEX_ELLIPSE2;
				} else {
					state = symbolNum();
				}
				break;
			case FLOAT_NUMBER_COMPLEX_ELLIPSE2:
				if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '(' || r == '[' || parenthesis(r) ||
						r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(r == '.') {
					r = read();
					state = State.FLOAT_NUMBER_COMPLEX_ELLIPSE3;
				} else {
					state = symbolNum();
				}
				break;
			case FLOAT_NUMBER_COMPLEX_ELLIPSE3:
				if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '(' || r == '[' || parenthesis(r) ||
						r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(isImaginary(r)) {
					appendBuf2(r);
					state = State.COMPLEX_I2;
					r = read();
				} else {
					state = symbolNum();
				}
				break;
			case FLOAT_NUMBER_E_COMPLEX:
				if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					state = popStateSymbolNum();
				} else if(parenthesis(r)) {
					state = popStateSymbolNum();
				} else if(r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(r == '+' || r == '-') {
					appendBuf2(r);
					state = State.FLOAT_NUMBER_E_SIGN_COMPLEX;
					r = read();
				} else if(isDigit09az(r)) {
					state = State.FLOAT_NUMBER_E_D_COMPLEX;
				} else {
					state = symbol2Num();
				}
				break;
			case FLOAT_NUMBER_E_SIGN_COMPLEX:
				if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					state = popStateSymbolNum();
				} else if(parenthesis(r)) {
					state = popStateSymbolNum();
				} else if(r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(isDigit09az(r)) {
					state = State.FLOAT_NUMBER_E_D_COMPLEX;
				} else {
					state = symbol2Num();
				}
				break;
			case FLOAT_NUMBER_E_D_COMPLEX:
				if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					state = popStateSymbolNum();
				} else if(parenthesis(r)) {
					state = popStateSymbolNum();
				} else if(r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(isDigit09az(r)) {
					appendBuf2(r);
					r = read();
				} else if(isImaginary(r)) {
					appendBuf2(r);
					state = State.COMPLEX_I2;
					r = read();
				} else {
					state = symbol2Num();
				}
				break;
			case RATIONAL_NUMBER_COMPLEX:
				if(isDigit09az(r)) {
					appendBuf2(r);
					r = read();
					state = State.RATIONAL_NUMBER_2_COMPLEX;
				} else if(r == '(' || r == '[') {
					state = popStateSymbolNum();
				} else if(parenthesis(r)) {
					state = popStateSymbolNum();
				} else if(r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else {
					state = symbol2Num();
				}
				break;
			case RATIONAL_NUMBER_2_COMPLEX:
				if(isDigit09az(r)) {
					appendBuf2(r);
					r = read();
				} else if(r == '(' || r == '[') {
					state = popStateSymbolNum();
				} else if(parenthesis(r)) {
					state = popStateSymbolNum();
				} else if(r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(isImaginary(r)) {
					appendBuf2(r);
					state = State.COMPLEX_I2;
					r = read();
				} else if(r == '+' || r == '-') {
					appendBuf2(r);
					state = State.CONTINUED_FRACTION_COMPLEX;
					r = read();
				} else {
					state = symbol2Num();
				}
				break;
			case CONTINUED_FRACTION_COMPLEX:
				if(r == '/') {
					appendBuf2(r);
					state = State.RATIONAL_NUMBER_COMPLEX;
					r = read();
				} else {
					state = symbol2Num();
				}
				break;
			case INF_I_COMPLEX:
				imgn = I_BIT;
				if(r == 'o') {
					imgn = imgn | L_BIT;
					r = read();
				}

				if(bufi[imgn] != null) {
					if(r == 'n') {
						appendBuf2(r);
						state = State.INF_N_COMPLEX;
						r = read();
					} else {
						state = symbol2Num();
					}
				} else if(r == EOF) {
					bufi[imgn] = buf2;
					pushResult(getComplex());
					state = popStateStack();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					bufi[imgn] = buf2;
					pushResult(getComplex());
					state = popStateStack();
				} else if(parenthesis(r)) {
					bufi[imgn] = buf2;
					pushResult(getComplex());
					state = popStateStack();
				} else if(r == ';') {
					bufi[imgn] = buf2;
					pushResult(getComplex());
					state = popStateStack();
				} else if(Character.isWhitespace(r)) {
					bufi[imgn] = buf2;
					pushResult(getComplex());
					state = popStateStack();
					r = readNext();
				} else if(r == 'n') {
					appendBuf2(r);
					state = State.INF_N_COMPLEX;
					r = read();
				} else if(r == '+' || r == '-') {
					bufi[imgn] = buf2;
					buf2 = new StringBuilder();
					appendBuf2(r);
					state = State.HEAD_SIGN_COMPLEX;
					r = read();
				} else {
					state = symbol2Num();
				}
				break;
			case INF_N_COMPLEX:
				if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					state = popStateSymbolNum();
				} else if(parenthesis(r)) {
					state = popStateSymbolNum();
				} else if(r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(r == 'f') {
					appendBuf2(r);
					state = State.INF_F_COMPLEX;
					r = read();
				} else {
					state = symbol2Num();
				}
				break;
			case INF_F_COMPLEX:
				if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					state = popStateSymbolNum();
				} else if(parenthesis(r)) {
					state = popStateSymbolNum();
				} else if(r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(r == '.') {
					appendBuf2(r);
					state = State.INF_DOT_COMPLEX;
					r = read();
				} else {
					state = symbol2Num();
				}
				break;
			case INF_DOT_COMPLEX:
				if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					state = popStateSymbolNum();
				} else if(parenthesis(r)) {
					state = popStateSymbolNum();
				} else if(r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(r == '0') {
					appendBuf2(r);
					state = State.INF_0_COMPLEX;
					r = read();
				} else {
					state = symbol2Num();
				}
				break;
			case INF_0_COMPLEX:
				if(r == EOF) {
					state = popStateSymbolNum();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					state = popStateSymbolNum();
				} else if(parenthesis(r)) {
					state = popStateSymbolNum();
				} else if(r == ';') {
					state = popStateSymbolNum();
				} else if(Character.isWhitespace(r)) {
					state = popStateSymbolNum();
					r = readNext();
				} else if(isImaginary(r)) {
					appendBuf2(r);
					state = State.COMPLEX_I2;
					r = read();
				} else {
					state = symbol2Num();
				}
				break;
			case COMPLEX_I2:
				r0   = buf2.charAt(buf2.length() - 1);
				imgn = getImaginaryNo(r0);
				if(r == 'o') {
					if(r0 == 'o') {
						state = symbol2Num();
						break;
					} else {
						imgn = imgn | L_BIT;
						r = read();
					}
				}

				if(bufi[imgn] != null) {
					state = symbol2Num();
					break;
				} else {
					bufi[imgn] = buf2;
				}

				if(r == '(' || r == '[') {
					pushResult(getComplex());
					state = popStateStack();
				} else if(parenthesis(r)) {
					pushResult(getComplex());
					state = popStateStack();
				} else if(r == ';') {
					pushResult(getComplex());
					state = popStateStack();
				} else if(Character.isWhitespace(r)) {
					pushResult(getComplex());
					state = popStateStack();
					r = readNext();
				} else if(r == '+' || r == '-') {
					buf2 = new StringBuilder();
					appendBuf2(r);
					state = State.HEAD_SIGN_COMPLEX;
					r = read();
				} else if(r == EOF) {
					pushResult(getComplex());
					state = popStateStack();
					return stateStack.empty();
				} else {
					state = symbol2Num();
				}
				break;
			case STRING:
				if(r == EOF) {
					if(consoleMode) {
						appendBuf('\n');
						return stateStack.empty();
					} else {
						throw message.getReadError("err.read.eof");
					}
				} else if(r == '\"') {
					pushResult(getString());
					state = popStateStack();
					r = readNext();
				} else if(r == '\\') {
					state = State.STRING_ESCAPE;
					r = read();
				} else {
					appendBuf(r);
					r = read();
				}
				break;
			case STRING_ESCAPE:
				switch(r) {
				case EOF:
					if(consoleMode) {
						state = State.STRING_NEWLINE2;
						return stateStack.empty();
					} else {
						throw message.getReadError("err.read.eof");
					}
				case 'a':
					appendBuf('\u0007');  state = State.STRING;
					r = read();
					break;
				case 'b':
					appendBuf('\u0008');  state = State.STRING;
					r = read();
					break;
				case 't':
					appendBuf('\u0009');  state = State.STRING;
					r = read();
					break;
				case 'n':
					appendBuf('\n');  state = State.STRING;
					r = read();
					break;
				case 'v':
					appendBuf('\u000b');  state = State.STRING;
					r = read();
					break;
				case 'f':
					appendBuf('\u000c');  state = State.STRING;
					r = read();
					break;
				case 'r':
					appendBuf('\r');  state = State.STRING;
					r = read();
					break;
				case 'x':  case 'u':
					state = State.STRING_CODE;  r = read();
					break;
				case '\n':
					state = State.STRING_NEWLINE2;  r = read();
					break;
				default:
					if(Character.isWhitespace(r)) {
						state = State.STRING_NEWLINE;
					} else {
						appendBuf(r);  state = State.STRING;
						r = read();
					}
					break;
				}
				break;
			case STRING_CODE:
				if((r >= '0' && r <= '9') ||
						(r >= 'a' && r <= 'f') ||
						(r >= 'A' && r <= 'F')) {
					buf2.append((char)r);  r = read();
				} else if(r == ';') {
					appendBuf(getByCharCode(getToken2()));
					buf2 = new StringBuilder();
					state = State.STRING;  r = read();
				} else if(buf2.length() == 4 || buf2.length() == 8) {
					appendBuf(getByCharCode(getToken2()));
					buf2 = new StringBuilder();
					state = State.STRING;
				} else {
					throw message.getReadError("err.read.string");
				}
				break;
			case STRING_NEWLINE:
				if(r == EOF) {
					if(consoleMode) {
						state = State.STRING_NEWLINE2;
						return stateStack.empty();
					} else {
						throw message.getReadError("err.read.eof");
					}
				} else if(Character.isWhitespace(r)) {
					r = read();
				} else if(r == '\n') {
					state = State.STRING_NEWLINE2;  r = read();
				} else {
					throw message.getReadError("err.read.string");
				}
				break;
			case STRING_NEWLINE2:
				if(r == EOF) {
					if(consoleMode) {
						appendBuf('\n');
						state = State.STRING;
						return stateStack.empty();
					} else {
						throw message.getReadError("err.read.eof");
					}
				} else if(Character.isWhitespace(r)) {
					r = read();
				} else {
					state = State.STRING;
				}
				break;
			case SHARP_SYNTAX_I:
				if(r == EOF) {
					throw message.getReadError(
							"err.read.sharp.unknown");
				} else if(r == '(' || r == '{') {
					parenStack.push(r);
					state = State.VECTOR;
					r = readNext();
				} else if(parenthesis(r)) {
					throw message.getReadError(
							"err.read.sharp.unknown");
				} else if(r == ';') {
					// s-expression comment
					state = State.S_EXP_COMMENT;
					r = readNext();
				} else if(r == '/') {
					// regex
					state = State.SHARP_SYNTAX;
					eng = ParserSharpSyntax.getInstance().getEngine();
					eng.go(r);
					r = read();
				} else if(Character.isWhitespace(r)) {
					throw message.getReadError("err.read.sharp.unknown");
				} else if(r == '|') {
					// block comment
					stateStack.push(State.INIT);
					state = State.BLOCK_COMMENT;
					r = readNext();
				} else if(r == '\\') {
					state = State.CHARACTER_EXP;
					r = read();
				} else if(r == 'e' || r == 'E') {
					prefixed = true;
					exactness = Exact.EXACT;
					state = State.NUMBER_PREFIX;
					r = read();
				} else if(r == 'i' || r == 'I') {
					prefixed = true;
					exactness = Exact.INEXACT;
					state = State.NUMBER_PREFIX;
					r = read();
				} else if(r == 'b' || r == 'B') {
					prefixed = true;
					radix = 2;
					state = State.NUMBER_PREFIX;
					r = read();
				} else if(r == 'o' || r == 'O') {
					prefixed = true;
					radix = 8;
					state = State.NUMBER_PREFIX;
					r = read();
				} else if(r == 'd' || r == 'D') {
					prefixed = true;
					radix = 10;
					state = State.NUMBER_PREFIX;
					r = read();
				} else if(r == 'x' || r == 'X') {
					prefixed = true;
					radix = 16;
					state = State.NUMBER_PREFIX;
					r = read();
				} else if(Character.isDigit(r)) {
					state = State.SHARP_SYNTAX_SRFI38_1;
					appendBuf(r);
					eng = ParserSharpSyntax.getInstance().getEngine();
					eng.go(r);
					r = read();
				} else if(r == 'r' || r == 'R') {
					state = State.SHARP_SYNTAX_RANGE;
					eng = ParserSharpSyntax.getInstance().getEngine();
					eng.go(r);
					r = read();
				} else if(r == '[') {
					state = State.SHARP_SYNTAX_CHARSET;  r = read();
				} else if(r == '!') {
					state = State.SHARP_HASH_BANG;  r = read();
				} else {
					state = State.SHARP_SYNTAX;
					eng = ParserSharpSyntax.getInstance().getEngine();
					eng.go(r);
					r = read();
				}
				break;
			case SHARP_SYNTAX_SRFI38_1:
				if(r == EOF) {
					throw message.getReadError("err.read.eol");
				} else if(Character.isDigit(r)) {
					appendBuf(r);
					eng.go(r);
					r = read();
				} else if(r == '=') {
					addNowNumber(
							Integer.parseInt(getTokenWithClear()));
					stateStack.push(State.SHARP_SYNTAX_SRFI38_2);
					state = State.INIT;
					eng.go(r);
					r = read();
				} else if(r == '#') {
					Datum x = definedDatum.get(
							Integer.parseInt(getTokenWithClear()));

					if(x == null) {
						throw message.getReadError(
								"err.read.srfi38.undefined");
					}
					pushResult(x);
					eng.go(r);
					r = read();
					state = popStateStack();
				} else {
					state = State.SHARP_SYNTAX;
					eng.go(r);
					r = read();
				}
				break;
			case SHARP_SYNTAX_RANGE:
				if(r == '(' || r == '[') {
					parenStack.push(r);
					state = State.RANGE;
					r = readNext();
				} else {
					state = State.SHARP_SYNTAX;
					eng = ParserSharpSyntax.getInstance().getEngine();
					eng.go(r);
					r = read();
				}
				break;
			case SHARP_SYNTAX:
				if(eng == null) {
					throw new NullPointerException();
				} else if(r == EOF || r == '(' || r == '[' ||
						parenthesis(r) || r == ';' ||
						Character.isWhitespace(r)) {
					if(eng.isMatch()) {
						if(!eng.isFollowS() && !eng.isUseMatch()) {
							pushResult(eng.getDatum());
							state = popStateStack();
							if(r == EOF) {
								return stateStack.empty();
							}
						} else if(eng.isFollowS() &&
								eng.isUseMatch()) {
							pushResult(eng.getDatum());
							pushResult(new LispString(
									eng.getMatchString()));
							stateStack.add(State.S_EXP_MACRO);
							state = State.INIT;
							if(r == EOF) {
								return stateStack.empty();
							}
						} else if(eng.isFollowS() &&
								!eng.isUseMatch()) {
							pushResult(eng.getDatum());
							stateStack.add(State.S_EXP_QUOTE);
							state = State.INIT;
							if(r == EOF) {
								return stateStack.empty();
							}
						} else {
							LispString ls =
								new LispString(eng.getMatchString());

							pushResult(new Cons(eng.getDatum(),
									new Cons(ls, Nil.NIL)));
							state = popStateStack();
							if(r == EOF) {
								return stateStack.empty();
							}
						}

						if(Character.isWhitespace(r)) {
							r = readNext();
						}
						break;
					} else if(r == EOF || eng.isDead()) {
						throw message.getReadError(
								"err.read.sharp.unknown");
					}
				}

				if(eng.isDeadNext(r)) {
					if(eng.isFollowS() && eng.isUseMatch()) {
						pushResult(eng.getDatum());
						pushResult(new LispString(
								eng.getMatchString()));
						stateStack.add(State.S_EXP_MACRO);
						state = State.INIT;
						break;
					} else if(eng.isFollowS() && !eng.isUseMatch()) {
						pushResult(eng.getDatum());
						stateStack.add(State.S_EXP_QUOTE);
						state = State.INIT;
						if(r == EOF) {
							return stateStack.empty();
						}
						break;
					}
				}
				eng.go(r);
				r = read();
				break;
			case SHARP_SYNTAX_RE:
				if(r == EOF) {
					pushResult(getRegex());
					state = popStateStack();
					return stateStack.empty();
				} else if(r == '/') {
					appendBuf(r);
					state = State.SHARP_SYNTAX_RE_F;
					r = read();
				} else {
					appendBuf(r);
					r = read();
				}
				break;
			case SHARP_SYNTAX_RE_F:
				if(r == EOF) {
					pushResult(getRegex());
					state = popStateStack();
					return stateStack.empty();
				} else if(r == '(' || r == '[') {
					pushResult(getRegex());
					state = popStateStack();
				} else if(parenthesis(r)) {
					pushResult(getRegex());
					state = popStateStack();
				} else if(r == ';') {
					pushResult(getRegex());
					state = popStateStack();
				} else if(Character.isWhitespace(r)) {
					pushResult(getRegex());
					state = popStateStack();
					r = readNext();
				} else if(r == '\\') {
					//state = State.CHARACTER_EXP;
					//r = read();
					throw message.getReadError(
							"err.read.sharp.unknown");
					//throw new ReadException("unsupported #-syntax");
				} else {
					appendBuf(r);
					r = read();
				}
				break;
			case SHARP_SYNTAX_CHARSET:
				if(r == ']') {
					pushResult(LispCharSet.parse(buf.toString()));
					state = popStateStack();
					r = readNext();
				} else if(r == '\\') {
					r = read();
					if(r == EOF) {
						throw message.getReadError("err.read.eol");
					}
					buf.append((char)r);
					r = read();
				} else {
					buf.append((char)r);
					r = read();
				}
				break;
			case SHARP_HASH_BANG:
				if(r == EOF) {
					processHashBang(buf2.toString());
					buf2 = new StringBuilder();
					state = popStateStack();
					return stateStack.empty();
				} else if(r == '\n') {
					processHashBang(buf2.toString());
					buf2 = new StringBuilder();
					state = popStateStack();
					r = readNext();
				} else {
					buf2.appendCodePoint(r);  r = read();
				}
				break;
			case CHARACTER_EXP:
				if(r == EOF) {
					throw message.getReadError("err.read.eol");
					//throw new ReadException("unexpected end of line");
				} else if(r == ';') {
					throw message.getReadError(
							"err.read.character.unknown");
					//throw new ReadException("bad character name");
				} else {
					appendBuf(r);
					state = State.CHARACTER_EXP_NAME;
					r = read();
				}
				break;
			case CHARACTER_EXP_NAME:
				if(r == '(' || r == '[') {
					pushResult(getCharacter());
					state = popStateStack();
				} else if(parenthesis(r)) {
					pushResult(getCharacter());
					state = popStateStack();
				} else if(r == ';') {
					pushResult(getCharacter());
					state = popStateStack();
				} else if(Character.isWhitespace(r)) {
					pushResult(getCharacter());
					state = popStateStack();
					r = readNext();
				} else if(r == EOF) {
					pushResult(getCharacter());
					state = popStateStack();
					return stateStack.empty();
				} else {
					appendBuf(r);
					r = read();
				}
				break;
			case SYMBOL:
				if(r == '(' || r == '[') {
					pushMaybeSymbol(getSymbol());
					state = popStateStack();
				} else if(parenthesis(r)) {
					pushMaybeSymbol(getSymbol());
					state = popStateStack();
				} else if(r == ';') {
					pushMaybeSymbol(getSymbol());
					state = popStateStack();
				} else if(Character.isWhitespace(r)) {
					pushMaybeSymbol(getSymbol());
					state = popStateStack();
					r = readNext();
				} else if(r == EOF) {
					pushMaybeSymbol(getSymbol());
					state = popStateStack();
					return stateStack.empty();
				} else {
					appendBuf(r);
					r = read();
				}
				break;
			case SYMBOL2:
				if(r == '(' || r == '[') {
					pushMaybeSymbol(getSymbol());
					state = popStateStack();
				} else if(parenthesis(r)) {
					pushMaybeSymbol(getSymbol());
					state = popStateStack();
				} else if(r == ';') {
					pushMaybeSymbol(getSymbol());
					state = popStateStack();
				} else if(Character.isWhitespace(r)) {
					pushMaybeSymbol(getSymbol());
					state = popStateStack();
					r = readNext();
				} else if(r == EOF) {
					pushMaybeSymbol(getSymbol());
					state = popStateStack();
					return stateStack.empty();
				} else {
					appendBuf2(r);
					r = read();
				}
				break;
			case LIST:
				if(r == EOF) {
					return stateStack.empty();
				} else if(r == ';') {
					stateStack.push(state);
					state = State.COMMENT;
					r = read();
				} else if(parenthesis(r)) {
					checkParenthesis(r);

					pushResult(Nil.NIL);
					state = popStateStack();
					r = readNext();
				} else {
					Cons n = new Cons(_VOID, Nil.NIL);

					pushResult(n);
					stateStack.push(State.LIST2);
					consStack.push(n);
					state = State.INIT;
				}
				break;
			case LIST2:
				if(r == EOF) {
					return stateStack.empty();
				} else if(r == ';') {
					stateStack.push(state);
					state = State.COMMENT;
					r = read();
				} else {
					Cons c1 = consStack.peek();
					Datum d1 = resultStack.pop();

					// s-expression comment
					if(d1 != _VOID) {
						c1.setCar(d1);
					}

					if(parenthesis(r)) {
						Datum d2 = resultStack.pop();

						checkParenthesis(r);

						// 空リストを判定する
						if(d2 instanceof Cons) {
							Cons cd2 = (Cons)d2;

							if(cd2.getCar() == _VOID) {
								pushResult(Nil.NIL);
							} else {
								pushResult(d2);
							}
						} else {
							throw new RuntimeException();
						}

						consStack.pop();
						state = popStateStack();
						r = readNext();
					} else if(r == '.') {
						appendBuf(r);
						state = State.LIST_DOT_INIT;
						r = read();
					} else {
						if(d1 != _VOID) {
							Cons n = new Cons();

							c1.setCdr(n);
							consStack.pop();
							consStack.push(n);
						}
						stateStack.push(State.LIST2);
						state = State.INIT;
					}
				}
				break;
			case LIST_DOT_INIT:
				Cons c3 = consStack.peek();

				if(c3.getCar() == _VOID) {
					throw message.getReadError("err.read.list.invalid");
					//throw new ReadException("invalid list");
				} else if(r == '(' || r == '[' ||
						parenthesis(r) || r == EOF) {
					stateStack.push(State.LIST_DOT);
					state = State.INIT;
					clearBuf();
					c3.setCdr(_VOID);
				} else if(r == ';') {
					stateStack.push(State.LIST_DOT);
					stateStack.push(State.INIT);
					state = State.COMMENT;
					clearBuf();
					c3.setCdr(_VOID);
				} else if(Character.isWhitespace(r)) {
					stateStack.push(State.LIST_DOT);
					state = State.INIT;
					r = readNext();
					c3.setCdr(_VOID);
				} else if(isDigit09(r)) {
					Cons n = new Cons();

					c3.setCdr(n);
					consStack.pop();
					stateStack.push(State.LIST2);
					consStack.push(n);
					state = State.FLOAT_NUMBER_D;
				} else {
					Cons n = new Cons();

					c3.setCdr(n);
					consStack.pop();
					stateStack.push(State.LIST2);
					consStack.push(n);
					state = State.SYMBOL;
				}
				break;
			case LIST_DOT:
				if(r == EOF) {
					return stateStack.empty();
				} else if(r == ';') {
					stateStack.push(state);
					state = State.COMMENT;
					r = read();
				} else {
					Cons c2 = consStack.peek();
					Datum d1 = resultStack.pop();

					// s-expression comment
					if(d1 != _VOID) {
						c2.setCdr(d1);
					}

					if(parenthesis(r)) {
						checkParenthesis(r);

						if(c2.getCdr() == _VOID) {
							throw message.getReadError("err.read.dot");
							//throw new ReadException("Bad dot syntax");
						}
						consStack.pop();
						state = popStateStack();
						r = readNext();
					} else if(d1 == _VOID) {
						stateStack.push(State.LIST_DOT);
						state = State.INIT;
						clearBuf();
					} else {
						//state = State.INIT;
						//resultStack.clear();
						//stateStack.clear();
						//consStack.clear();
						throw message.getReadError("err.read.dot");
						//throw new ReadException("Bad dot syntax");
					}
				}
				break;
			case QUOTE:
				Cons n1 = new Cons();
				Cons n2 = new Cons();

				if(r == '@') {
					appendBuf(r);
					r = readNext2();
				}

				n1.setCar(getQuote());
				n1.setCdr(n2);
				pushResult(n1);
				consStack.push(n2);
				stateStack.push(State.QUOTE);
				state = State.INIT;
				break;
			case VECTOR:
				if(r == EOF) {
					return stateStack.empty();
				} else if(r == ')' || r == '}') {
					checkParenthesis(r);
					switch(r) {
					case ')':
						pushResult(LispVector.EMPTY);
						break;
					case '}':
						pushResult(new LispHashSet());
						break;
					}
					state = popStateStack();
					r = readNext();
				} else if(r == ']') {
					throw message.getReadError(
							"err.read.parenthesis.mismatch");
				} else if(r == ';') {
					stateStack.push(state);
					state = State.COMMENT;
					r = read();
				} else {
					vectorStack.push(new ArrayList<Datum>());
					stateStack.push(State.VECTOR2);
					state = State.INIT;
				}
				break;
			case VECTOR2:
				if(r == EOF) {
					return stateStack.empty();
				} else if(r == ';') {
					stateStack.push(state);
					state = State.COMMENT;
					r = read();
				} else {
					List<Datum> v1 = vectorStack.peek();
					Datum d1 = resultStack.pop();

					// s-expression comment
					if(d1 != _VOID) {
						v1.add(d1);
					}

					if(r == ')') {
						checkParenthesis(r);
						vectorStack.pop();
						pushResult(new LispVector(v1));
						state = popStateStack();
						r = readNext();
					} else if(r == '}') {
						checkParenthesis(r);
						vectorStack.pop();
						pushResult(new LispHashSet(v1));
						state = popStateStack();
						r = readNext();
					} else if(r == ']') {
						throw message.getReadError(
								"err.read.parenthesis.mismatch");
					} else {
						stateStack.push(State.VECTOR2);
						state = State.INIT;
					}
				}
				break;
			case COMMENT:
				if(r == EOF) {
					state = stateStack.pop();
					//return stateStack.empty();
				} else if(r == '\n') {
					state = stateStack.pop();
					r = readNext();
				} else {
					r = read();
				}
				break;
			case BLOCK_COMMENT:
				if(r == EOF) {
					return stateStack.empty();
				} else if(r == '|') {
					r = read();
					if(r == '#') {
						state = stateStack.pop();
						r = readNext();
					} else {
						r = readNext();
					}
				} else if(r == '#') {
					r = read();
					if(r == '|') {
						stateStack.push(State.BLOCK_COMMENT);
						r = readNext();
					} else {
						r = readNext();
					}
				} else {
					r = readNext();
				}
				break;
			case S_EXP_COMMENT:
				stateStack.push(State.S_EXP_COMMENT);
				state = State.INIT;
				break;
			case SYMBOLQUOTE:
				if(r == EOF || r == '(' || r == '[' ||
						Character.isWhitespace(r)) {
					pushResult(Symbol.getSymbolWithoutFoldCase("|"));
					state = popStateStack();
					r = readNext();
				} else if(r == '|') {
//					throw message.getError("err.read.symbol.invalid");
					appendBuf(r);
					r = read();
					state = State.SYMBOL;
				} else {
					state = State.SYMBOLQUOTE2;
				}
				break;
			case SYMBOLQUOTE2:
				if(r == EOF) {
					throw message.getReadError("err.read.eof");
				} else if(r == '|') {
					pushResult(Symbol.getSymbolWithoutFoldCase(
							getToken()));
					state = popStateStack();
					r = readNext();
				} else {
					appendBuf(r);
					r = read();
				}
				break;
			case RANGE:
				if(r == EOF) {
					return stateStack.empty();
				} else if(r == ')' || r == ']') {
					//pushResult(LispBoolean.FALSE);
					pushResult(LispRealNumberSet.O);
					state = popStateStack();
					r = readNext();
				} else if(r == ';') {
					stateStack.push(state);
					state = State.COMMENT;
					r = read();
				} else {
					stateStack.push(State.RANGE2);
					state = State.INIT;
				}
				break;
			case RANGE2:
				if(r == EOF) {
					return stateStack.empty();
				} else if(r == ';') {
					stateStack.push(state);
					state = State.COMMENT;
					r = read();
				} else if(r == ')' || r == ']') {
					pushResult(getRange(r, resultStack.pop()));
					state = popStateStack();
					r = readNext();
				} else {
					rangeStack.push(resultStack.pop());
					stateStack.push(State.RANGE3);
					state = State.INIT;
				}
				break;
			case RANGE3:
				if(r == EOF) {
					return stateStack.empty();
				} else if(r == ';') {
					stateStack.push(state);
					state = State.COMMENT;
					r = read();
				} else if(r == ')' || r == ']') {
					pushResult(getRange(r,
							rangeStack.pop(), resultStack.pop()));
					state = popStateStack();
					r = readNext();
				} else {
					throw message.getError("err.read.range.type");
				}
				break;
			case END_OF_FILE:
				throw message.getError("err.read.eof");
			}
		}
	}

}
