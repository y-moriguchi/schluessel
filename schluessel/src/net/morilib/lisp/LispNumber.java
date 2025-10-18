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

import java.math.BigDecimal;
import java.math.BigInteger;

import net.morilib.lang.Decimal32;
import net.morilib.lang.DoubleUtils;
import net.morilib.lisp.math.ILispExactOrInexactQuantity;
import net.morilib.lisp.math.algebra.ILispField;
import net.morilib.lisp.math.algebra.ILispNorm;
import net.morilib.lisp.math.algebra.ILispNumberEqual;
import net.morilib.lisp.sos.LispType;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public abstract class LispNumber extends Atom
implements ILispField<LispNumber>, ILispNorm,
ILispExactOrInexactQuantity<LispNumber>,
ILispNumberEqual<LispNumber> {

	//
	private static final int I_BIT = 0x1;
	//private static final int J_BIT = 0x2;
	//private static final int K_BIT = 0x3;
	private static final int L_BIT = 0x4;

	/*package*/ static String disp(double n) {
		if(n == Double.POSITIVE_INFINITY) {
			return "+inf.0";
		} else if(n == Double.NEGATIVE_INFINITY) {
			return "-inf.0";
		} else if(Double.isNaN(n)) {
			return "+nan.0";
		} else {
			return Double.toString(n);
		}
	}

	//
	private static enum State {
		INIT,                           // 初期状態
		NUMBER_PREFIX,                  // 数値のprefix
		HEAD_SIGN,                      // 先頭の符号
		NUMBER,                         // 整数
		FLOAT_NUMBER,                   // 浮動小数点開始
		FLOAT_NUMBER_D,                 // 浮動小数点
		FLOAT_NUMBER_E,                 // 浮動小数点指数部開始
		FLOAT_NUMBER_E_SIGN,            // 浮動小数点指数部符号
		FLOAT_NUMBER_E_D,               // 浮動小数点指数部
		RATIONAL_NUMBER,                // 有理数開始
		RATIONAL_NUMBER_2,              // 有理数
		INF_I,                          // 無限
		INF_N,
		INF_F,
		INF_DOT,
		INF_0,
		NAN_N1,                          // NaN
		NAN_A,
		NAN_N2,
		NAN_DOT,
		NAN_0,
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
		FLOAT_NUMBER_D_COMPLEX,         // 浮動小数点(複素数)
		FLOAT_NUMBER_E_COMPLEX,         // 浮動小数点指数部開始(複素数)
		FLOAT_NUMBER_E_SIGN_COMPLEX,    // 浮動小数点指数部符号(複素数)
		FLOAT_NUMBER_E_D_COMPLEX,       // 浮動小数点指数部(複素数)
		RATIONAL_NUMBER_COMPLEX,        // 有理数開始(複素数)
		RATIONAL_NUMBER_2_COMPLEX,      // 有理数(複素数)
		INF_I_COMPLEX,                  // 無限(複素数)
		INF_N_COMPLEX,
		INF_F_COMPLEX,
		INF_DOT_COMPLEX,
		INF_0_COMPLEX,
		COMPLEX_I2,                     // 複素数単位(i)
	};

	private enum Exact {
		DEFAULT,
		EXACT,
		INEXACT,
	}

	private static boolean isAlnum(int r, int radix) {
		if(radix <= 10) {
			return (r >= '0' && r <= '9');
		} else if(radix <= 14) {
			return ((r >= '0' && r <= '9') ||
					(r >= 'a' && r <= 'd') || (r >= 'A' && r <= 'D'));
		} else {
			return ((r >= '0' && r <= '9') ||
					(r >= 'a' && r <= 'f') || (r >= 'A' && r <= 'F'));
		}
	}

	//
	private static boolean isImaginary(int r) {
		return r == 'i' || r == 'j' || r == 'k' || r == 'o';
	}

	//
	private static int getImaginaryNo(int r) {
		switch(r) {
		case 'i':  return 1;
		case 'j':  return 2;
		case 'k':  return 3;
		case 'o':  return 4;
		default:   throw new RuntimeException();
		}
	}

	private static int read(String s, int p) {
		return (p >= s.length()) ? -1 : s.charAt(p);
	}

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

	private static double getValueByDouble(String str) {
		double i;
		String[] ind = str.split("/");

		if(ind.length == 2) {
			double n = parseDouble(ind[0]);
			double d = parseDouble(ind[1]);

			i = n / d;
		} else if(ind.length == 1) {
			i = parseDouble(ind[0]);
		} else {
			throw new RuntimeException("internal error");
		}
		return i;
	}

	private static BigInteger getValueByBigInt(String str, int r) {
		return new BigInteger(str.replaceFirst("^\\+", ""), r);
	}

	private static LispNumber getInteger(
			String str, int radix, Exact exact) {
		BigInteger num;
		LispInteger li;

		try {
			num = getValueByBigInt(str, radix);
			li  = LispInteger.valueOf(num);
			return Exact.INEXACT.equals(exact) ? li.toInexact() : li;
		} catch(NumberFormatException e) {
			return null;
		}
	}

	private static LispNumber getRational(
			String str, int radix, Exact exact) {
		String[] nd = str.split("/");
		BigInteger num, den;
		LispNumber li;

		try {
			num = getValueByBigInt(nd[0], radix);
			den = getValueByBigInt(nd[1], radix);
			li = LispRational.newRational(num, den);
			return Exact.INEXACT.equals(exact) ? li.toInexact() : li;
		} catch(NumberFormatException e) {
			return null;
		}
	}

	private static LispNumber getDouble(
			String str, int radix, Exact ex) {
		double val;
		LispDouble atom;
		int r = (radix < 0) ? 10 : radix;

		if(r != 10) {
			return null;
			//throw new ReadException("only radix 10 is supported");
		}

		val = parseDouble(str);
		atom = new LispDouble(val);
		if(Exact.EXACT.equals(ex)) {
			return atom.toExact();
		} else {
			return atom;
		}
	}

	private static LispNumber getImaginary(
			String gtok1, int rx, Exact ex, int imgn) {
		String iii = gtok1.replaceFirst("[ijko][ijk]?$", "");
		LispReal im;

		if(rx != 10) {
			return null;
		}

		// imaginary part
		if("+".equals(iii)) {
			im = !Exact.INEXACT.equals(ex) ?
					LispInteger.ONE : new LispDouble(1.0);
		} else if("-".equals(iii)) {
			im = !Exact.INEXACT.equals(ex) ?
					LispInteger.ONE.uminus() : new LispDouble(-1.0);
		} else if(iii.indexOf('/') >= 0) {
			BigInteger num, den;
			String[] nd = iii.split("/");

			try {
				num = getValueByBigInt(nd[0], rx);
				den = getValueByBigInt(nd[1], rx);
				if(Exact.INEXACT.equals(ex)) {
					double numd = num.doubleValue();
					double dend = den.doubleValue();

					im = new LispDouble(numd / dend);
				} else {
					im = LispRational.newRational(num, den);
				}
			} catch(NumberFormatException e) {
				return null;
			}
		} else if(!Exact.INEXACT.equals(ex) &&
				iii.matches("^[-+]?[0-9]+$")) {
			iii = iii.replaceFirst("^\\+", "");
			im = LispInteger.valueOf(getValueByBigInt(iii, rx));
		} else {
			im = new LispDouble(getValueByDouble(iii));
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

	private static LispNumber getComplex(
			String gtok1, String[] bufi, int rx, Exact ex) {
		LispReal re;
		LispReal[] im = new LispReal[8];
		String iii;

		if(rx > 0 && rx != 10) {
			return null;
		}

		// real part
		if(gtok1.indexOf('/') >= 0) {
			BigInteger num, den;
			String[] nd = gtok1.split("/");

			try {
				num = getValueByBigInt(nd[0], rx);
				den = getValueByBigInt(nd[1], rx);
				if(Exact.INEXACT.equals(ex)) {
					double numd = num.doubleValue();
					double dend = den.doubleValue();

					re = new LispDouble(numd / dend);
				} else {
					re = LispRational.newRational(num, den);
				}
			} catch(NumberFormatException e) {
				return null;
			}
		} else if(!Exact.INEXACT.equals(ex) &&
				gtok1.matches("^[-+]?[0-9]+$")) {
			re = LispInteger.valueOf(getValueByBigInt(gtok1, rx));
		} else {
			re = new LispDouble(getValueByDouble(gtok1));
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
				BigInteger num, den;
				String[] nd = iii.split("/");
	
				try {
					num = getValueByBigInt(nd[0], rx);
					den = getValueByBigInt(nd[1], rx);
					if(Exact.INEXACT.equals(ex)) {
						double numd = num.doubleValue();
						double dend = den.doubleValue();
	
						im[i] = new LispDouble(numd / dend);
					} else {
						im[i] = LispRational.newRational(num, den);
					}
				} catch(NumberFormatException e) {
					return null;
				}
			} else if(!Exact.INEXACT.equals(ex) &&
					iii.matches("^[-+]?[0-9]+$")) {
				iii = iii.replaceFirst("^\\+", "");
				im[i] = LispInteger.valueOf(getValueByBigInt(iii, rx));
			} else {
				im[i] = new LispDouble(getValueByDouble(iii));
			}
		}

		return LispOctonion.newOctonion(
				re,    im[1], im[2], im[3],
				im[4], im[5], im[6], im[7]);
	}

	private static LispNumber getPolar(
			String str, int radix, Exact exact) {
		double r;
		String[] ra = str.split("@");

		if(radix != 10) {
			return null;
		} else if(Exact.EXACT.equals(exact)) {
			return null;
		}

		r = getValueByDouble(ra[0]);    // 半径
		if(r == 0.0) {
			return new LispDouble(0.0);
		} else {
			double a = getValueByDouble(ra[1]);

			return LispComplex.newComplex(
					r * Math.cos(a), r * Math.sin(a));
		}
	}

	/**
	 * 
	 * @param s
	 * @param radix
	 * @return
	 */
	public static LispNumber parse(String s, int radix) {
		StringBuilder b1 = new StringBuilder();
		StringBuilder b2 = new StringBuilder();
		String[] bi = new String[8];
		State state = State.INIT;
		boolean radix2 = false;
		Exact exact = Exact.DEFAULT;
		boolean signed = false;
		int p = 0;
		int r = read(s, p);
		int r0, imgn;

		if(radix < 1 || radix > 16) {
			throw new IllegalArgumentException("wrong radix");
		}

		while(true) {
			switch(state) {
			case INIT:
				if(r == '-' || r == '+') {
					b1.append((char)r);
					state = State.HEAD_SIGN;
					r = read(s, ++p);
				} else if(isAlnum(r, radix)) {
					b1.append((char)r);
					state = State.NUMBER;
					r = read(s, ++p);
				} else if(r == '.') {
					b1.append((char)r);
					state = State.FLOAT_NUMBER;
					r = read(s, ++p);
				} else if(r == '#') {
					state = State.NUMBER_PREFIX;
				} else if(r < 0) {
					return null;
				} else {
					return null;
				}
				break;
			case NUMBER_PREFIX:
				if(r < 0) {
					return null;
				} else if(r == '#') {
					r = read(s, ++p);

					if(r == 'e' || r == 'E') {
						if(!Exact.DEFAULT.equals(exact)) {
							return null;
						}
						exact = Exact.EXACT;
						state = State.NUMBER_PREFIX;
					} else if(r == 'i' || r == 'I') {
						if(!Exact.DEFAULT.equals(exact)) {
							return null;
						}
						exact = Exact.INEXACT;
						state = State.NUMBER_PREFIX;
					} else if(r == 'b' || r == 'B') {
						if(radix2) {
							return null;
						}
						radix = 2;
						radix2 = true;
						state = State.NUMBER_PREFIX;
					} else if(r == 'o' || r == 'O') {
						if(radix2) {
							return null;
						}
						radix = 8;
						radix2 = true;
						state = State.NUMBER_PREFIX;
					} else if(r == 'd' || r == 'D') {
						if(radix2) {
							return null;
						}
						radix = 10;
						radix2 = true;
						state = State.NUMBER_PREFIX;
					} else if(r == 'x' || r == 'X') {
						if(radix2) {
							return null;
						}
						radix = 16;
						radix2 = true;
						state = State.NUMBER_PREFIX;
					}
					r = read(s, ++p);
				} else if(r == '-' || r == '+') {
					b1.append((char)r);
					state = State.HEAD_SIGN;
					r = read(s, ++p);
				} else if(isAlnum(r, radix)) {
					b1.append((char)r);
					state = State.NUMBER;
					r = read(s, ++p);
				} else if(r == '.') {
					b1.append((char)r);
					state = State.FLOAT_NUMBER;
					r = read(s, ++p);
				} else if(isImaginary(r)) {
					b1.append((char)r);
					state = State.COMPLEX_I;
					r = read(s, ++p);
				} else {
					return null;
				}
				break;
			case HEAD_SIGN:
				signed = true;

				if(isAlnum(r, radix)) {
					state = State.NUMBER;
				} else if(r == '.') {
					b1.append((char)r);
					state = State.FLOAT_NUMBER;
					r = read(s, ++p);
				} else if(r == 'i') {
					b1.append((char)r);
					state = State.INF_I;
					r = read(s, ++p);
				} else if(isImaginary(r)) {
					b1.append((char)r);
					state = State.COMPLEX_I;
					r = read(s, ++p);
				} else if(r == 'n') {
					b1.append((char)r);
					state = State.NAN_N1;
					r = read(s, ++p);
				} else if(r == '@') {
					b1.append((char)r);
					state = State.INIT_POLAR;
					r = read(s, ++p);
				} else if(r < 0) {
					return null;
				} else {
					return null;
				}
				break;
			case NUMBER:
				if(isAlnum(r, radix)) {
					b1.append((char)r);
					r = read(s, ++p);
				} else if(r == '.') {
					b1.append((char)r);
					state = State.FLOAT_NUMBER;
					r = read(s, ++p);
				} else if(r == 'e' || r == 'E') {
					b1.append((char)r);
					state = State.FLOAT_NUMBER_E;
					r = read(s, ++p);
				} else if(r == '/') {
					b1.append((char)r);
					state = State.RATIONAL_NUMBER;
					r = read(s, ++p);
				} else if(r == '+' || r == '-') {
					b2.append((char)r);
					state = State.HEAD_SIGN_COMPLEX;
					r = read(s, ++p);
				} else if(isImaginary(r)) {
					b1.append((char)r);
					state = State.COMPLEX_I;
					r = read(s, ++p);
				} else if(r == '@') {
					b1.append((char)r);
					state = State.INIT_POLAR;
					r = read(s, ++p);
				} else if(r < 0) {
					return getInteger(b1.toString(), radix, exact);
				} else {
					return null;
				}
				break;
			case FLOAT_NUMBER:
				if(isAlnum(r, radix)) {
					state = State.FLOAT_NUMBER_D;
				} else if(r < 0) {
					return null;
				} else {
					return null;
				}
				break;
			case FLOAT_NUMBER_D:
				if(isAlnum(r, radix)) {
					b1.append((char)r);
					r = read(s, ++p);
				} else if(r == 'e' || r == 'E') {
					b1.append((char)r);
					state = State.FLOAT_NUMBER_E;
					r = read(s, ++p);
				} else if(r == '+' || r == '-') {
					b2.append((char)r);
					state = State.HEAD_SIGN_COMPLEX;
					r = read(s, ++p);
				} else if(isImaginary(r)) {
					b1.append((char)r);
					state = State.COMPLEX_I;
					r = read(s, ++p);
				} else if(r == '@') {
					b1.append((char)r);
					state = State.INIT_POLAR;
					r = read(s, ++p);
				} else if(r < 0) {
					return getDouble(b1.toString(), radix, exact);
				} else {
					return null;
				}
				break;
			case FLOAT_NUMBER_E:
				if(r == '+' || r == '-') {
					b1.append((char)r);
					state = State.FLOAT_NUMBER_E_SIGN;
					r = read(s, ++p);
				} else if(isAlnum(r, radix)) {
					state = State.FLOAT_NUMBER_E_D;
				} else if(r < 0) {
					return null;
				} else {
					return null;
				}
				break;
			case FLOAT_NUMBER_E_SIGN:
				if(isAlnum(r, radix)) {
					state = State.FLOAT_NUMBER_E_D;
				} else if(r < 0) {
					return null;
				} else {
					return null;
				}
				break;
			case FLOAT_NUMBER_E_D:
				if(isAlnum(r, radix)) {
					b1.append((char)r);
					r = read(s, ++p);
				} else if(r == '+' || r == '-') {
					b2.append((char)r);
					state = State.HEAD_SIGN_COMPLEX;
					r = read(s, ++p);
				} else if(isImaginary(r)) {
					b1.append((char)r);
					state = State.COMPLEX_I;
					r = read(s, ++p);
				} else if(r == '@') {
					b1.append((char)r);
					state = State.INIT_POLAR;
					r = read(s, ++p);
				} else if(r < 0) {
					return getDouble(b1.toString(), radix, exact);
				} else {
					return null;
				}
				break;
			case RATIONAL_NUMBER:
				if(isAlnum(r, radix)) {
					b1.append((char)r);
					r = read(s, ++p);
					state = State.RATIONAL_NUMBER_2;
				} else if(r < 0) {
					return null;
				} else {
					return null;
				}
				break;
			case RATIONAL_NUMBER_2:
				if(isAlnum(r, radix)) {
					b1.append((char)r);
					r = read(s, ++p);
				} else if(r == '+' || r == '-') {
					b2.append((char)r);
					state = State.HEAD_SIGN_COMPLEX;
					r = read(s, ++p);
				} else if(isImaginary(r)) {
					b1.append((char)r);
					state = State.COMPLEX_I;
					r = read(s, ++p);
				} else if(r == '@') {
					b1.append((char)r);
					state = State.INIT_POLAR;
					r = read(s, ++p);
				} else if(r < 0) {
					return getRational(b1.toString(), radix, exact);
				} else {
					return null;
				}
				break;
			case INIT_POLAR:
				if(r == '-' || r == '+') {
					b1.append((char)r);
					state = State.HEAD_SIGN_POLAR;
					r = read(s, ++p);
				} else if(isAlnum(r, radix)) {
					b1.append((char)r);
					state = State.NUMBER_POLAR;
					r = read(s, ++p);
				} else if(r == '.') {
					b1.append((char)r);
					state = State.FLOAT_NUMBER_POLAR;
					r = read(s, ++p);
				} else if(r < 0) {
					return null;
				} else {
					return null;
				}
				break;
			case HEAD_SIGN_POLAR:
				if(isAlnum(r, radix)) {
					state = State.NUMBER_POLAR;
				} else if(r == '.') {
					b1.append((char)r);
					state = State.FLOAT_NUMBER_POLAR;
					r = read(s, ++p);
				} else if(r < 0) {
					return null;
				} else {
					return null;
				}
				break;
			case NUMBER_POLAR:
				if(isAlnum(r, radix)) {
					b1.append((char)r);
					r = read(s, ++p);
				} else if(r == '.') {
					b1.append((char)r);
					state = State.FLOAT_NUMBER_POLAR;
					r = read(s, ++p);
				} else if(r == 'e' || r == 'E') {
					b1.append((char)r);
					state = State.FLOAT_NUMBER_E_POLAR;
					r = read(s, ++p);
				} else if(r == '/') {
					b1.append((char)r);
					state = State.RATIONAL_NUMBER_POLAR;
					r = read(s, ++p);
				} else if(r < 0) {
					return getPolar(b1.toString(), radix, exact);
				} else {
					return null;
				}
				break;
			case FLOAT_NUMBER_POLAR:
				if(isAlnum(r, radix)) {
					state = State.FLOAT_NUMBER_D_POLAR;
				} else if(r < 0) {
					return null;
				} else {
					return null;
				}
				break;
			case FLOAT_NUMBER_D_POLAR:
				if(isAlnum(r, radix)) {
					b1.append((char)r);
					r = read(s, ++p);
				} else if(r == 'e' || r == 'E') {
					b1.append((char)r);
					state = State.FLOAT_NUMBER_E_POLAR;
					r = read(s, ++p);
				} else if(r < 0) {
					return getPolar(b1.toString(), radix, exact);
				} else {
					return null;
				}
				break;
			case FLOAT_NUMBER_E_POLAR:
				if(r == '+' || r == '-') {
					b1.append((char)r);
					state = State.FLOAT_NUMBER_E_SIGN_POLAR;
					r = read(s, ++p);
				} else if(isAlnum(r, radix)) {
					state = State.FLOAT_NUMBER_E_D_POLAR;
				} else if(r < 0) {
					return null;
				} else {
					return null;
				}
				break;
			case FLOAT_NUMBER_E_SIGN_POLAR:
				if(isAlnum(r, radix)) {
					state = State.FLOAT_NUMBER_E_D_POLAR;
				} else if(r < 0) {
					return null;
				} else {
					return null;
				}
				break;
			case FLOAT_NUMBER_E_D_POLAR:
				if(isAlnum(r, radix)) {
					b1.append((char)r);
					r = read(s, ++p);
				} else if(r < 0) {
					return getPolar(b1.toString(), radix, exact);
				} else {
					return null;
				}
				break;
			case RATIONAL_NUMBER_POLAR:
				if(isAlnum(r, radix)) {
					b1.append((char)r);
					r = read(s, ++p);
					state = State.RATIONAL_NUMBER_2_POLAR;
				} else if(r < 0) {
					return null;
				} else {
					return null;
				}
				break;
			case RATIONAL_NUMBER_2_POLAR:
				if(isAlnum(r, radix)) {
					b1.append((char)r);
					r = read(s, ++p);
				} else if(r < 0) {
					return getPolar(b1.toString(), radix, exact);
				} else {
					return null;
				}
				break;
			case INF_I:
				imgn = I_BIT;
				if(r == 'o') {
					imgn = imgn | L_BIT;
					r = read(s, ++p);
				}

				if(r < 0) {
					return getImaginary(
							b1.toString(), radix, exact, imgn);
				} else if(r == 'n') {
					b1.append((char)r);
					state = State.INF_N;
					r = read(s, ++p);
				} else if(r == '+' || r == '-') {
					bi[I_BIT] = b1.toString();
					b1 = new StringBuilder("0");
					b2.append((char)r);
					state = State.HEAD_SIGN_COMPLEX;
					r = read(s, ++p);
				} else {
					return null;
				}
				break;
			case INF_N:
				if(r == 'f') {
					b1.append((char)r);
					state = State.INF_F;
					r = read(s, ++p);
				} else {
					return null;
				}
				break;
			case INF_F:
				if(r == '.') {
					b1.append((char)r);
					state = State.INF_DOT;
					r = read(s, ++p);
				} else {
					return null;
				}
				break;
			case INF_DOT:
				if(r == '0') {
					b1.append((char)r);
					state = State.INF_0;
					r = read(s, ++p);
				} else {
					return null;
				}
				break;
			case INF_0:
				if(r < 0) {
					String s1 = b1.toString();

					if(s1.charAt(0) == '+') {
						return LispDouble.POSITIVE_INFINITY;
					} else {
						return LispDouble.NEGATIVE_INFINITY;
					}
				} else if(r == '+' || r == '-') {
					b2.append((char)r);
					state = State.HEAD_SIGN_COMPLEX;
					r = read(s, ++p);
				} else if(isImaginary(r)) {
					b1.append((char)r);
					state = State.COMPLEX_I;
					r = read(s, ++p);
				} else if(r == '@') {
					b1.append((char)r);
					state = State.INIT_POLAR;
					r = read(s, ++p);
				} else {
					return null;
				}
				break;
			case NAN_N1:
				if(r == 'a') {
					b1.append((char)r);
					state = State.NAN_A;
					r = read(s, ++p);
				} else {
					return null;
				}
				break;
			case NAN_A:
				if(r == 'n') {
					b1.append((char)r);
					state = State.NAN_N2;
					r = read(s, ++p);
				} else {
					return null;
				}
				break;
			case NAN_N2:
				if(r == '.') {
					b1.append((char)r);
					state = State.NAN_DOT;
					r = read(s, ++p);
				} else {
					return null;
				}
				break;
			case NAN_DOT:
				if(r == '0') {
					b1.append((char)r);
					state = State.NAN_0;
					r = read(s, ++p);
				} else {
					return null;
				}
				break;
			case NAN_0:
				if(r < 0) {
					return LispDouble.NaN;
				} else {
					return null;
				}
				//break;
			case COMPLEX_I:
				r0   = b1.charAt(b1.length() - 1);
				imgn = getImaginaryNo(r0);
				if(r == 'o') {
					if(r0 == 'o') {
						return null;
					} else {
						imgn = imgn | L_BIT;
						r = read(s, ++p);
					}
				}
				bi[imgn] = b1.toString();

				if(r < 0) {
					if(signed) {
						return getImaginary(
								b1.toString(), radix, exact, imgn);
					} else {
						return null;
					}
				} else if(r == '+' || r == '-') {
					bi[imgn] = b1.toString();
					b1 = new StringBuilder("0");
					b2.append((char)r);
					state = State.HEAD_SIGN_COMPLEX;
					r = read(s, ++p);
				} else {
					return null;
				}
			case HEAD_SIGN_COMPLEX:
				if(isAlnum(r, radix)) {
					state = State.NUMBER_COMPLEX;
				} else if(r == '.') {
					b2.append((char)r);
					state = State.FLOAT_NUMBER_COMPLEX;
					r = read(s, ++p);
				} else if(r == 'i') {
					b2.append((char)r);
					state = State.INF_I_COMPLEX;
					r = read(s, ++p);
				} else if(isImaginary(r)) {
					b2.append((char)r);
					state = State.COMPLEX_I2;
					r = read(s, ++p);
				} else if(r < 0) {
					return null;
				} else {
					return null;
				}
				break;
			case NUMBER_COMPLEX:
				if(isAlnum(r, radix)) {
					b2.append((char)r);
					r = read(s, ++p);
				} else if(r == '.') {
					b2.append((char)r);
					state = State.FLOAT_NUMBER_COMPLEX;
					r = read(s, ++p);
				} else if(r == 'e' || r == 'E') {
					b2.append((char)r);
					state = State.FLOAT_NUMBER_E_COMPLEX;
					r = read(s, ++p);
				} else if(r == '/') {
					b2.append((char)r);
					state = State.RATIONAL_NUMBER_COMPLEX;
					r = read(s, ++p);
				} else if(isImaginary(r)) {
					b2.append((char)r);
					state = State.COMPLEX_I2;
					r = read(s, ++p);
				} else if(r < 0) {
					return null;
				} else {
					return null;
				}
				break;
			case FLOAT_NUMBER_COMPLEX:
				if(isAlnum(r, radix)) {
					state = State.FLOAT_NUMBER_D_COMPLEX;
				} else if(r < 0) {
					return null;
				} else {
					return null;
				}
				break;
			case FLOAT_NUMBER_D_COMPLEX:
				if(isAlnum(r, radix)) {
					b2.append((char)r);
					r = read(s, ++p);
				} else if(r == 'e' || r == 'E') {
					b2.append((char)r);
					state = State.FLOAT_NUMBER_E_COMPLEX;
					r = read(s, ++p);
				} else if(isImaginary(r)) {
					b2.append((char)r);
					state = State.COMPLEX_I2;
					r = read(s, ++p);
				} else if(r < 0) {
					return null;
				} else {
					return null;
				}
				break;
			case FLOAT_NUMBER_E_COMPLEX:
				if(r == '+' || r == '-') {
					b2.append((char)r);
					state = State.FLOAT_NUMBER_E_SIGN_COMPLEX;
					r = read(s, ++p);
				} else if(isAlnum(r, radix)) {
					state = State.FLOAT_NUMBER_E_D_COMPLEX;
				} else if(r < 0) {
					return null;
				} else {
					return null;
				}
				break;
			case FLOAT_NUMBER_E_SIGN_COMPLEX:
				if(isAlnum(r, radix)) {
					state = State.FLOAT_NUMBER_E_D_COMPLEX;
				} else if(r < 0) {
					return null;
				} else {
					return null;
				}
				break;
			case FLOAT_NUMBER_E_D_COMPLEX:
				if(isAlnum(r, radix)) {
					b2.append((char)r);
					r = read(s, ++p);
				} else if(isImaginary(r)) {
					b2.append((char)r);
					state = State.COMPLEX_I2;
					r = read(s, ++p);
				} else if(r < 0) {
					return null;
				} else {
					return null;
				}
				break;
			case RATIONAL_NUMBER_COMPLEX:
				if(isAlnum(r, radix)) {
					b2.append((char)r);
					r = read(s, ++p);
					state = State.RATIONAL_NUMBER_2_COMPLEX;
				} else if(r < 0) {
					return null;
				} else {
					return null;
				}
				break;
			case RATIONAL_NUMBER_2_COMPLEX:
				if(isAlnum(r, radix)) {
					b2.append((char)r);
					r = read(s, ++p);
				} else if(isImaginary(r)) {
					b2.append((char)r);
					state = State.COMPLEX_I2;
					r = read(s, ++p);
				} else if(r < 0) {
					return null;
				} else {
					return null;
				}
				break;
			case INF_I_COMPLEX:
				imgn = I_BIT;
				if(r == 'o') {
					imgn = imgn | L_BIT;
					r = read(s, ++p);
				}

				if(bi[imgn] != null) {
					return null;
				}

				if(r < 0) {
					bi[imgn] = b2.toString();
					return getComplex(b1.toString(), bi, radix, exact);
				} else if(r == 'n') {
					b2.append((char)r);
					state = State.INF_N_COMPLEX;
					r = read(s, ++p);
				} else if(r == '+' || r == '-') {
					bi[imgn] = b2.toString();
					b2 = new StringBuilder();
					b2.append((char)r);
					state = State.HEAD_SIGN_COMPLEX;
					r = read(s, ++p);
				} else {
					return null;
				}
				break;
			case INF_N_COMPLEX:
				if(r == 'f') {
					b2.append((char)r);
					state = State.INF_F_COMPLEX;
					r = read(s, ++p);
				} else {
					return null;
				}
				break;
			case INF_F_COMPLEX:
				if(r == '.') {
					b2.append((char)r);
					state = State.INF_DOT_COMPLEX;
					r = read(s, ++p);
				} else {
					return null;
				}
				break;
			case INF_DOT_COMPLEX:
				if(r == '0') {
					b2.append((char)r);
					state = State.INF_0_COMPLEX;
					r = read(s, ++p);
				} else {
					return null;
				}
				break;
			case INF_0_COMPLEX:
				if(isImaginary(r)) {
					b2.append((char)r);
					state = State.COMPLEX_I2;
					r = read(s, ++p);
				} else {
					return null;
				}
				break;
			case COMPLEX_I2:
				r0   = b2.charAt(b2.length() - 1);
				imgn = getImaginaryNo(r0);
				if(r == 'o') {
					if(r0 == 'o') {
						return null;
					} else {
						imgn = imgn | L_BIT;
						r = read(s, ++p);
					}
				}

				if(bi[imgn] != null) {
					return null;
				}
				bi[imgn] = b2.toString();

				if(r < 0) {
					return getComplex(b1.toString(), bi, radix, exact);
				} else if(r == '+' || r == '-') {
					b2 = new StringBuilder();
					b2.append((char)r);
					state = State.HEAD_SIGN_COMPLEX;
					r = read(s, ++p);
				} else {
					return null;
				}
			}
		}
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public abstract LispNumber add(LispNumber x);

	/**
	 * 
	 * @param x
	 * @return
	 */
	public abstract LispNumber sub(LispNumber x);

	/**
	 * 
	 * @param x
	 * @return
	 */
	public abstract LispNumber mul(LispNumber x);

	/**
	 * 
	 * @param x
	 * @return
	 */
	public abstract LispNumber div(LispNumber x);

	/**
	 * 
	 * @return
	 */
	public abstract LispNumber uminus();

	/**
	 * 
	 * @param x
	 * @return
	 */
	public abstract boolean isEqualTo(LispNumber x);

	/**
	 * 
	 * @return
	 */
	public abstract boolean isInteger();

	/**
	 * 
	 * @return
	 */
	public abstract boolean isRational();

	/**
	 * 
	 * @return
	 */
	public abstract boolean isReal();

	/**
	 * 
	 * @return
	 */
	public abstract boolean isExact();

	/**
	 * 
	 * @return
	 */
	public abstract LispNumber toExact();

	/**
	 * 
	 * @return
	 */
	public abstract LispNumber toInexact();

	/**
	 * 
	 * @param radix
	 * @return
	 */
	public abstract LispString toLispString(int radix);

	/**
	 * 
	 * @param radix
	 * @param precision
	 * @return
	 */
	public abstract LispString toLispString(int radix, int precision);

	/**
	 * 
	 * @return
	 */
	public abstract LispReal norm();

	/**
	 * 
	 * @return
	 */
	public abstract LispNumber conjugate();

	/**
	 * 
	 * @return
	 */
	public abstract boolean isNaN();

	/**
	 * 
	 * @return
	 */
	public abstract boolean isOne();

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getInt()
	 */
	public abstract int getInt();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getLong()
	 */
	public abstract long getLong();

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getBigInteger()
	 */
	public abstract BigInteger getBigInteger();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getBigDecimal()
	 */
	public abstract BigDecimal getBigDecimal();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getRealDouble()
	 */
	public abstract double getRealDouble();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getRealFloat()
	 */
	public float getRealFloat() {
		return (float)getRealDouble();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getRealHalf()
	 */
	public short getRealHalf() {
		return DoubleUtils.toHalf(getRealDouble());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getRealDouble()
	 */
	public abstract long getRealDecimal64();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getRealDecimal32()
	 */
	public int getRealDecimal32() {
		return Decimal32.decimal64To32(getRealDecimal64());
	}

	/**
	 * 
	 * @return
	 */
	public abstract BigInteger getNumerator();

	/**
	 * 
	 * @return
	 */
	public abstract BigInteger getDenominator();

	/**
	 * 
	 * @return
	 */
	public abstract LispReal[] getImags();

	/**
	 * 
	 * @return
	 */
	public abstract double[] getImagsDouble();

	/**
	 * 
	 * @return
	 */
	public abstract LispReal[] getImagsAsQuaternion();

	/**
	 * 
	 * @return
	 */
	public abstract double[] getImagsDoubleAsQuaternion();

	/**
	 * 
	 * @return
	 */
	public abstract LispReal[] getImagsAsOctonion();

	/**
	 * 
	 * @return
	 */
	public abstract double[] getImagsDoubleAsOctonion();

	/**
	 * 
	 * @param n
	 * @return
	 */
	public LispNumber pow(int n) {
		LispNumber r = LispInteger.ONE;

		if(n >= 0) {
			for(int i = 0; i < n; i++) {
				r = r.mul(this);
			}
		} else {
			for(int i = 0; i < -n; i++) {
				r = r.div(this);
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispInvertable#invert()
	 */
	public LispNumber inv() {
		return LispInteger.ONE.div(this);
	}

	/**
	 * 
	 * @return
	 */
	public int getExactSmallInt() {
		throw new NumberFormatException();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Atom#toLispString()
	 */
	public LispString toLispString() {
		return toLispString(10);
	}

	/**
	 * 
	 * @return
	 */
	public boolean isZero() {
		return isEqualTo(LispInteger.ZERO);
	}

	/**
	 * 
	 * @return
	 */
	public abstract boolean isFinite();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getType()
	 */
	@Override
	public LispType getType() {
		return LispType.NUMBER;
	}

}
