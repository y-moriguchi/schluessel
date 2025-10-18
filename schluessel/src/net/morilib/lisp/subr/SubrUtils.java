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
package net.morilib.lisp.subr;

import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Keyword;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispCharacter;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.LispString;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Symbol;
import net.morilib.util.Iterators;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class SubrUtils {

	//
	private static final BigInteger MINLONG =
		BigInteger.valueOf(Long.MIN_VALUE);
	private static final BigInteger MAXLONG =
		BigInteger.valueOf(Long.MAX_VALUE);

	//
	private SubrUtils() {}

	/**
	 * 
	 * @param n
	 * @param d
	 * @return
	 */
	public static BigInteger ceil(BigInteger n, BigInteger d) {
		BigInteger v = n;

		if(v.compareTo(BigInteger.ZERO) > 0) {
			v = v.add(d);
		}
		return v.divide(d);
	}

	/**
	 * 
	 * @param n
	 * @param d
	 * @return
	 */
	public static BigInteger floor(BigInteger n, BigInteger d) {
		BigInteger v = n;

		if(v.compareTo(BigInteger.ZERO) < 0) {
			v = v.subtract(d);
		}
		return v.divide(d);
	}

	/**
	 * 
	 * @param i
	 * @return
	 */
	public static Integer toIntExact(BigInteger i) {
		int r = i.intValue();

		if(i.equals(BigInteger.valueOf(r))) {
			return Integer.valueOf(r);
		} else {
			return null;
		}
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static int getSmallInt(Datum d, LispMessage mesg) {
		/*if(d instanceof LispInteger) {
			LispInteger li = (LispInteger)d;
			Integer i = SubrUtils.toIntExact(li.bigIntegerValue());

			if(i == null) {
				throw new LispException("small integer required");
			}
			return i.intValue();
		} else {
			throw new LispException("small integer required");
		}*/
		if(d instanceof LispSmallInt) {
			LispSmallInt li = (LispSmallInt)d;

			return li.getExactSmallInt();
		} else {
//			throw new LispException("small integer required");
			throw mesg.getError("err.require.smallint", d);
		}
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static int getNonnegativeSmallInt(Datum d,
			LispMessage mesg) {
		if(d instanceof LispSmallInt) {
			LispSmallInt li = (LispSmallInt)d;
			int k = li.getExactSmallInt();

			if(k < 0) {
				throw mesg.getError("err.require.int.nonnegative", d);
			}
			return k;
		} else {
			throw mesg.getError("err.require.int.nonnegative", d);
		}
	}

	/**
	 * 
	 * @param d
	 * @param mesg
	 * @return
	 */
	public static long getLongExact(Datum d, LispMessage mesg) {
		if(d instanceof LispSmallInt) {
			return ((LispSmallInt)d).getExactSmallInt();
		} else if(d instanceof LispInteger) {
			BigInteger b = d.getBigInteger();

			if(b.compareTo(MINLONG) < 0 || b.compareTo(MAXLONG) > 0) {
				throw mesg.getError("err.require.longint", d);
			}
			return d.getLong();
		} else {
			throw mesg.getError("err.require.longint", d);
		}
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static char getCharacter(Datum d, LispMessage mesg) {
		if(d instanceof LispCharacter) {
			return ((LispCharacter)d).getCharacter();
		} else {
			throw mesg.getError("err.require.char", d);
		}
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static int getCharacterCodePoint(Datum d,
			LispMessage mesg) {
		if(d instanceof LispCharacter) {
			return ((LispCharacter)d).getCharacterCodePoint();
		} else {
			throw mesg.getError("err.require.char", d);
		}
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static String getString(Datum d, LispMessage mesg) {
		if(d instanceof LispString) {
			return ((LispString)d).getString();
		} else {
			throw mesg.getError("err.require.string", d);
		}
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static double getDouble(Datum d, LispMessage mesg) {
		if(d instanceof LispReal) {
			return d.getRealDouble();
		} else {
			throw mesg.getError("err.require.real", d);
		}
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static float getFloat(Datum d, LispMessage mesg) {
		if(d instanceof LispReal) {
			return d.getRealFloat();
		} else {
			throw mesg.getError("err.require.real", d);
		}
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static short getHalf(Datum d, LispMessage mesg) {
		if(d instanceof LispReal) {
			return d.getRealHalf();
		} else {
			throw mesg.getError("err.require.real", d);
		}
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static int getDecimal32(Datum d, LispMessage mesg) {
		if(d instanceof LispReal) {
			return d.getRealDecimal32();
		} else {
			throw mesg.getError("err.require.real", d);
		}
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static long getDecimal64(Datum d, LispMessage mesg) {
		if(d instanceof LispReal) {
			return d.getRealDecimal64();
		} else {
			throw mesg.getError("err.require.real", d);
		}
	}

	/**
	 * 
	 * @param d
	 * @param mesg
	 * @return
	 */
	public static String getSymbolName(Datum d, LispMessage mesg) {
		if(d instanceof Symbol) {
			return ((Symbol)d).getName();
		} else {
			throw mesg.getError("err.require.symbol", d);
		}
	}

	/**
	 * @param next
	 * @param mesg
	 * @return
	 */
	public static Symbol getSymbol(Datum d, LispMessage mesg) {
		if(d instanceof Symbol) {
			return (Symbol)d;
		} else {
			throw mesg.getError("err.require.symbol", d);
		}
	}

	/**
	 * 
	 * @param d
	 * @param mesg
	 * @return
	 */
	public static Procedure getProcedure(Datum d, LispMessage mesg) {
		if(d instanceof Procedure) {
			return (Procedure)d;
		} else {
			throw mesg.getError("err.require.procedure", d);
		}
	}

	/**
	 * 
	 * @param d
	 * @param mesg
	 * @return
	 */
	public static int getU8(Datum d, LispMessage mesg) {
		int c = getSmallInt(d, mesg);

		if(c < 0 || c > 255) {
			throw mesg.getError("err.uvector.outofrange.u8", d);
		}
		return c;
	}

	/**
	 * 
	 * @param d
	 * @param mesg
	 * @return
	 */
	public static int getS8(Datum d, LispMessage mesg) {
		int c = getSmallInt(d, mesg);

		if(c < -128 || c > 127) {
			throw mesg.getError("err.uvector.outofrange.s8", d);
		}
		return c;
	}

	/**
	 * 
	 * @param d
	 * @param mesg
	 * @return
	 */
	public static int getU16(Datum d, LispMessage mesg) {
		int c = getSmallInt(d, mesg);

		if(c < 0 || c > 65535) {
			throw mesg.getError("err.uvector.outofrange.u16", d);
		}
		return c;
	}

	/**
	 * 
	 * @param d
	 * @param mesg
	 * @return
	 */
	public static int getS16(Datum d, LispMessage mesg) {
		int c = getSmallInt(d, mesg);

		if(c < -32768 || c > 32767) {
			throw mesg.getError("err.uvector.outofrange.s16", d);
		}
		return c;
	}

	/**
	 * 
	 * @param itr
	 * @param body
	 * @param mesg
	 */
	public static void checkTerminated(
			ConsIterator itr, Datum body, LispMessage mesg) {
		if(itr.hasNext()) {
			throw mesg.getError("err.argument", body);
		} else if(!itr.getTerminal().isNil()) {
			throw mesg.getError("err.list", body);
		}
	}

	/**
	 * 
	 * @param itr
	 * @param body
	 * @param mesg
	 */
	public static void checkProper(
			ConsIterator itr, Datum body, LispMessage mesg) {
		if(!itr.rest().isNil()) {
			throw mesg.getError("err.list", body);
		}
	}

	/**
	 * 
	 * @param itr
	 * @param mesg
	 * @param errcd
	 * @param ed
	 * @return
	 */
	public static Datum nextIf(
			Iterator<Datum> itr, LispMessage mesg, String errcd) {
		return Iterators.nextIf(itr, mesg.getError(errcd));
	}

	/**
	 * 
	 * @param itr
	 * @param mesg
	 * @param errcd
	 * @param ed
	 * @return
	 */
	public static Datum nextIf(
			Iterator<Datum> itr,
			LispMessage mesg, String errcd, String ed) {
		return Iterators.nextIf(itr, mesg.getError(errcd, ed));
	}

	/**
	 * 
	 * @param itr
	 * @param mesg
	 * @param errcd
	 * @param ed
	 * @return
	 */
	public static Datum nextIf(
			Iterator<Datum> itr,
			LispMessage mesg, String errcd, Datum ed) {
		return Iterators.nextIf(itr, mesg.getError(errcd, ed));
	}

	/**
	 * 
	 * @param itr
	 * @param mesg
	 * @param body
	 * @return
	 */
	public static Datum nextIf(
			Iterator<Datum> itr, LispMessage mesg, Datum body) {
		return Iterators.nextIf(itr,
				mesg.getError("err.argument", body));
	}

	/**
	 * 
	 * @param itr
	 * @param s
	 * @param mesg
	 * @return
	 */
	public static String nextString(
			Iterator<Datum> itr, String s, LispMessage mesg) {
		Datum d = Iterators.nextIf(itr, (Datum)null);

		if(d == null) {
			return s;
		} else if(d instanceof LispString) {
			return d.getString();
		} else {
			throw mesg.getError("err.require.string", d);
		}
	}

	/**
	 * 
	 * @param itr
	 * @param mesg
	 * @param body
	 * @return
	 */
	public static String nextString(
			Iterator<Datum> itr, LispMessage mesg, Datum body) {
		Datum d = Iterators.nextIf(itr, (Datum)null);

		if(d == null) {
			throw mesg.getError("err.argument", body);
		} else if(d instanceof LispString) {
			return d.getString();
		} else {
			throw mesg.getError("err.require.string", d);
		}
	}

	/**
	 * 
	 * @param itr
	 * @param s
	 * @param mesg
	 * @return
	 */
	public static String nextSymbolName(
			Iterator<Datum> itr, String s, LispMessage mesg) {
		Datum d = Iterators.nextIf(itr, (Datum)null);

		if(d == null) {
			return s;
		} else if(d instanceof Symbol) {
			return ((Symbol)d).getName();
		} else {
			throw mesg.getError("err.require.symbol", d);
		}
	}

	/**
	 * 
	 * @param itr
	 * @param mesg
	 * @param body
	 * @return
	 */
	public static String nextSymbolName(
			Iterator<Datum> itr, LispMessage mesg, Datum body) {
		Datum d = Iterators.nextIf(itr, (Datum)null);

		if(d == null) {
			throw mesg.getError("err.argument", body);
		} else if(d instanceof Symbol) {
			return ((Symbol)d).getName();
		} else {
			throw mesg.getError("err.require.symbol", d);
		}
	}

	/**
	 * 
	 * @param itr
	 * @param s
	 * @param mesg
	 * @return
	 */
	public static int nextSmallInt(
			Iterator<Datum> itr, int s, LispMessage mesg) {
		Datum d = Iterators.nextIf(itr, (Datum)null);

		if(d == null) {
			return s;
		} else if(d instanceof LispSmallInt) {
			return d.getInt();
		} else {
			throw mesg.getError("err.require.smallint", d);
		}
	}

	/**
	 * 
	 * @param itr
	 * @param mesg
	 * @param body
	 * @return
	 */
	public static int nextSmallInt(
			Iterator<Datum> itr, LispMessage mesg, Datum body) {
		Datum d = Iterators.nextIf(itr, (Datum)null);

		if(d == null) {
			throw mesg.getError("err.argument", body);
		} else if(d instanceof LispSmallInt) {
			return d.getInt();
		} else {
			throw mesg.getError("err.require.smallint", d);
		}
	}

	/**
	 * 
	 * @param itr
	 * @param s
	 * @param mesg
	 * @return
	 */
	public static int nextNonnegativeSmallInt(
			Iterator<Datum> itr, int s, LispMessage mesg) {
		Datum d = Iterators.nextIf(itr, (Datum)null);

		if(d == null) {
			return s;
		} else {
			return getNonnegativeSmallInt(d, mesg);
		}
	}

	/**
	 * 
	 * @param itr
	 * @param mesg
	 * @param body
	 * @return
	 */
	public static int nextNonnegativeSmallInt(
			Iterator<Datum> itr, LispMessage mesg, Datum body) {
		Datum d = Iterators.nextIf(itr, (Datum)null);

		if(d == null) {
			throw mesg.getError("err.argument", body);
		} else {
			return getNonnegativeSmallInt(d, mesg);
		}
	}

	/**
	 * 
	 * @param itr
	 * @param mesg
	 * @param body
	 * @return
	 */
	public static String nextKeywordName(
			Iterator<Datum> itr, LispMessage mesg, Datum body) {
		Datum d = Iterators.nextIf(itr, (Datum)null);

		if(d == null) {
			throw mesg.getError("err.argument", body);
		} else if(d instanceof Keyword) {
			return ((Keyword)d).getName();
		} else {
			throw mesg.getError("err.require.keyword", d);
		}
	}

	/**
	 * 
	 * @param itr
	 * @param mesg
	 * @param body
	 * @return
	 */
	public static LispReal nextReal(
			Iterator<Datum> itr, LispMessage mesg, Datum body) {
		Datum d = Iterators.nextIf(itr, (Datum)null);

		if(d == null) {
			throw mesg.getError("err.argument", body);
		} else if(d instanceof LispReal) {
			return (LispReal)d;
		} else {
			throw mesg.getError("err.require.real", d);
		}
	}

	/**
	 * 
	 * @param itr
	 * @param mesg
	 * @param body
	 * @return
	 */
	public static LispReal nextReal(
			Iterator<Datum> itr, LispReal def) {
		Datum d = Iterators.nextIf(itr, (Datum)null);

		return (d instanceof LispReal) ? (LispReal)d : def;
	}

	/**
	 * 
	 * @param itr
	 * @param mesg
	 * @param body
	 * @return
	 */
	public static Procedure nextProcedure(
			Iterator<Datum> itr, LispMessage mesg, Datum body) {
		Datum d = Iterators.nextIf(itr, (Datum)null);

		if(d == null) {
			throw mesg.getError("err.argument", body);
		} else if(d instanceof Procedure) {
			return (Procedure)d;
		} else {
			throw mesg.getError("err.require.procedure", d);
		}
	}

	/**
	 * 
	 * @param itr
	 * @param mesg
	 * @param body
	 * @return
	 */
	public static Procedure nextProcedureOptional(
			Iterator<Datum> itr, LispMessage mesg) {
		Datum d = Iterators.nextIf(itr, (Datum)null);

		if(d == null || !d.isTrue()) {
			return null;
		} else if(d instanceof Procedure) {
			return (Procedure)d;
		} else {
			throw mesg.getError("err.require.procedure", d);
		}
	}

	/**
	 * 
	 * @param itr
	 * @param mesg
	 * @param body
	 * @return
	 */
	public static double nextDouble(
			Iterator<Datum> itr, LispMessage mesg, Datum body) {
		Datum d = Iterators.nextIf(itr, (Datum)null);

		if(d == null) {
			throw mesg.getError("err.argument", body);
		} else if(d instanceof LispReal) {
			return ((LispReal)d).doubleValue();
		} else {
			throw mesg.getError("err.require.real", d);
		}
	}

	/**
	 * 
	 * @param itr
	 * @param mesg
	 * @param body
	 * @return
	 */
	public static double nextDouble(
			Iterator<Datum> itr, double def) {
		Datum d = Iterators.nextIf(itr, (Datum)null);

		return (d instanceof LispReal) ?
				((LispReal)d).doubleValue() : def;
	}

	/**
	 * 
	 * @param xx
	 * @param d
	 * @return
	 */
	public static Datum cxxr(String xx, Datum d) {
		Datum p = d;

		for(int i = xx.length() - 1; i >= 0; i--) {
			switch(xx.charAt(i)) {
			case 'a':
				if(p instanceof Cons) {
					p = ((Cons)p).getCar();
				} else {
					return null;
				}
				break;
			case 'd':
				if(p instanceof Cons) {
					p = ((Cons)p).getCdr();
				} else {
					return null;
				}
				break;
			default:
				throw new IllegalArgumentException(xx);
			}
		}
		return p;
	}

	/**
	 * 
	 * @param d
	 * @param mesg
	 * @param msg
	 * @return
	 */
	public static Datum unwrap(Datum d, LispMessage mesg, String m) {
		if(d instanceof Cons) {
			Cons c1 = (Cons)d;

			if(c1.getCdr().isNil()) {
				return c1.getCar();
			} else {
				throw mesg.getError(m, d);
			}
		} else {
			throw mesg.getError(m, d);
		}
	}

	/**
	 * @param d
	 * @return
	 */
	public static Map<Keyword, Datum> keywordArgsToMap(
			Datum d, Datum body, LispMessage mesg) {
		Map<Keyword, Datum> res = new HashMap<Keyword, Datum>();
		ConsIterator itr = new ConsIterator(d);

		while(itr.hasNext()) {
			Datum d1;

			if(!((d1 = itr.next()) instanceof Keyword)) {
				throw mesg.getError("err.require.keyword", d1);
			} else if(!itr.hasNext()) {
				throw mesg.getError("err.argument", body);
			} else {
				res.put((Keyword)d1, itr.next());
			}
		}

		if(!itr.getTerminal().isNil()) {
			throw mesg.getError("err.list", body);
		}
		return res;
	}

	/**
	 * @param c1a
	 * @param class1
	 * @param mesg
	 * @param string
	 */
	public static void checkType(Datum c1a, Class<?> class1,
			LispMessage mesg, String string) {
		if(!class1.isInstance(c1a)) {
			throw mesg.getError(string, c1a);
		}
	}

	/**
	 * 
	 * @param x
	 * @param mesg
	 * @return
	 */
	public static Datum car(Datum x, LispMessage mesg) {
		Datum x1 = x;

		if(x1 instanceof Cons) {
			return ((Cons)x1).getCar();
		}
		throw mesg.getError("err.require.pair", x1);
	}

	/**
	 * 
	 * @param x
	 * @param mesg
	 * @return
	 */
	public static Datum cadr(Datum x, LispMessage mesg) {
		Datum x1 = x;

		if(x1 instanceof Cons) {
			x1 = ((Cons)x1).getCdr();
			if(x1 instanceof Cons) {
				return ((Cons)x1).getCar();
			}
		}
		throw mesg.getError("err.require.pair", x1);
	}

	/**
	 * 
	 * @param x
	 * @param mesg
	 * @return
	 */
	public static Datum caddr(Datum x, LispMessage mesg) {
		Datum x1 = x;

		if(x1 instanceof Cons) {
			x1 = ((Cons)x1).getCdr();
			if(x1 instanceof Cons) {
				x1 = ((Cons)x1).getCdr();
				if(x1 instanceof Cons) {
					return ((Cons)x1).getCar();
				}
			}
		}
		throw mesg.getError("err.require.pair", x1);
	}

	/**
	 * 
	 * @param x
	 * @param mesg
	 * @return
	 */
	public static Datum cadddr(Datum x, LispMessage mesg) {
		Datum x1 = x;

		if(x1 instanceof Cons) {
			x1 = ((Cons)x1).getCdr();
			if(x1 instanceof Cons) {
				x1 = ((Cons)x1).getCdr();
				if(x1 instanceof Cons) {
					x1 = ((Cons)x1).getCdr();
					if(x1 instanceof Cons) {
						return ((Cons)x1).getCar();
					}
				}
			}
		}
		throw mesg.getError("err.require.pair", x1);
	}

	/**
	 * 
	 * @param x
	 * @param mesg
	 * @return
	 */
	public static Datum cdr(Datum x, LispMessage mesg) {
		Datum x1 = x;

		if(x1 instanceof Cons) {
			return ((Cons)x1).getCdr();
		}
		throw mesg.getError("err.require.pair", x1);
	}

	/**
	 * 
	 * @param x
	 * @param mesg
	 * @return
	 */
	public static Datum cddr(Datum x, LispMessage mesg) {
		Datum x1 = x;

		if(x1 instanceof Cons) {
			x1 = ((Cons)x1).getCdr();
			if(x1 instanceof Cons) {
				return ((Cons)x1).getCdr();
			}
		}
		throw mesg.getError("err.require.pair", x1);
	}

	/**
	 * 
	 * @param x
	 * @param mesg
	 * @return
	 */
	public static Datum cdddr(Datum x, LispMessage mesg) {
		Datum x1 = x;

		if(x1 instanceof Cons) {
			x1 = ((Cons)x1).getCdr();
			if(x1 instanceof Cons) {
				x1 = ((Cons)x1).getCdr();
				if(x1 instanceof Cons) {
					return ((Cons)x1).getCdr();
				}
			}
		}
		throw mesg.getError("err.require.pair", x1);
	}

	/**
	 * 
	 * @param x
	 * @param mesg
	 * @return
	 */
	public static Datum cddddr(Datum x, LispMessage mesg) {
		Datum x1 = x;

		if(x1 instanceof Cons) {
			x1 = ((Cons)x1).getCdr();
			if(x1 instanceof Cons) {
				x1 = ((Cons)x1).getCdr();
				if(x1 instanceof Cons) {
					x1 = ((Cons)x1).getCdr();
					if(x1 instanceof Cons) {
						return ((Cons)x1).getCdr();
					}
				}
			}
		}
		throw mesg.getError("err.require.pair", x1);
	}

	/**
	 * 
	 * @param x
	 * @param mesg
	 * @return
	 */
	public static void cdrNull(Datum x, LispMessage mesg) {
		Datum x1 = x;

		if(x1 instanceof Cons) {
			x1 = ((Cons)x1).getCdr();
			if(!x1.isNil()) {
				throw mesg.getError("err.require.nil", x1);
			} else {
				return;
			}
		}
		throw mesg.getError("err.require.pair", x1);
	}

	/**
	 * 
	 * @param x
	 * @param mesg
	 * @return
	 */
	public static void cddrNull(Datum x, LispMessage mesg) {
		Datum x1 = x;

		if(x1 instanceof Cons) {
			x1 = ((Cons)x1).getCdr();
			if(x1 instanceof Cons) {
				x1 = ((Cons)x1).getCdr();
				if(!x1.isNil()) {
					throw mesg.getError("err.require.nil", x1);
				} else {
					return;
				}
			}
		}
		throw mesg.getError("err.require.pair", x1);
	}

	/**
	 * 
	 * @param x
	 * @param mesg
	 * @return
	 */
	public static void cdddrNull(Datum x, LispMessage mesg) {
		Datum x1 = x;

		if(x1 instanceof Cons) {
			x1 = ((Cons)x1).getCdr();
			if(x1 instanceof Cons) {
				x1 = ((Cons)x1).getCdr();
				if(x1 instanceof Cons) {
					x1 = ((Cons)x1).getCdr();
					if(!x1.isNil()) {
						throw mesg.getError("err.require.nil", x1);
					} else {
						return;
					}
				}
			}
		}
		throw mesg.getError("err.require.pair", x1);
	}

	/**
	 * 
	 * @param x
	 * @param mesg
	 * @return
	 */
	public static void cddddrNull(Datum x, LispMessage mesg) {
		Datum x1 = x;

		if(x1 instanceof Cons) {
			x1 = ((Cons)x1).getCdr();
			if(x1 instanceof Cons) {
				x1 = ((Cons)x1).getCdr();
				if(x1 instanceof Cons) {
					x1 = ((Cons)x1).getCdr();
					if(x1 instanceof Cons) {
						x1 = ((Cons)x1).getCdr();
						if(!x1.isNil()) {
							throw mesg.getError("err.require.nil", x1);
						} else {
							return;
						}
					}
				}
			}
		}
		throw mesg.getError("err.require.pair", x1);
	}

	/**
	 * 
	 * @param x
	 * @param mesg
	 * @return
	 */
	public static Datum cndr(Datum x, int n, LispMessage mesg) {
		Datum x1 = x;

		for(int i = 0; i < n; i++) {
			if(x1 instanceof Cons) {
				x1 = ((Cons)x1).getCdr();
			} else {
				return null;
			}
		}
		return x1;
	}

	/**
	 * 
	 * @param x
	 * @param mesg
	 * @return
	 */
	public static LispReal getReal(Datum x, LispMessage mesg) {
		if(x instanceof LispReal) {
			return (LispReal)x;
		} else {
			throw mesg.getError("err.require.real", x);
		}
	}

	/**
	 * 
	 * @param x
	 * @param mesg
	 * @return
	 */
	public static LispInteger getInteger(Datum x, LispMessage mesg) {
		if(x instanceof LispInteger) {
			return (LispInteger)x;
		} else {
			throw mesg.getError("err.require.int", x);
		}
	}

	/**
	 * 
	 * @param x
	 * @param mesg
	 * @return
	 */
	public static BigInteger[] getRational(Datum x, LispMessage mesg) {
		if(x instanceof LispReal && ((LispReal)x).isRational()) {
			return new BigInteger[] {
					((LispReal)x).getNumerator(),
					((LispReal)x).getDenominator()
			};
		} else {
			throw mesg.getError("err.require.rational", x);
		}
	}

	/**
	 * 
	 * @param x
	 * @param mesg
	 * @return
	 */
	public static BigInteger getBigInteger(Datum x, LispMessage mesg) {
		if(x instanceof LispInteger) {
			return x.getBigInteger();
		} else {
			throw mesg.getError("err.require.int", x);
		}
	}

	/**
	 * 
	 * @param x
	 * @param mesg
	 * @return
	 */
	public static LispNumber getNumber(Datum x, LispMessage mesg) {
		if(x instanceof LispNumber) {
			return (LispNumber)x;
		} else {
			throw mesg.getError("err.require.number", x);
		}
	}

	/**
	 * 
	 * @param itr
	 * @param mesg
	 * @param body
	 * @return
	 */
	public static int nextCharacter(
			Iterator<Datum> itr, LispMessage mesg, Datum body) {
		Datum d = Iterators.nextIf(itr, (Datum)null);

		if(d == null) {
			throw mesg.getError("err.argument", body);
		} else if(d instanceof LispCharacter) {
			return d.getCharacterCodePoint();
		} else {
			throw mesg.getError("err.require.char", d);
		}
	}

	/**
	 * 
	 * @param itr
	 * @param mesg
	 * @param body
	 * @return
	 */
	public static String nextStringOptional(
			Iterator<Datum> itr, LispMessage mesg, Datum body,
			String def) {
		Datum d = Iterators.nextIf(itr, (Datum)null);

		if(d == null || !d.isTrue()) {
			return def;
		} else if(d instanceof LispString) {
			return d.getString();
		} else {
			throw mesg.getError("err.require.string", d);
		}
	}

	/**
	 * 
	 * @param itr
	 * @param mesg
	 * @param body
	 * @return
	 */
	public static int nextCharacterOptional(
			Iterator<Datum> itr, LispMessage mesg, Datum body,
			int def) {
		Datum d = Iterators.nextIf(itr, (Datum)null);

		if(d == null || !d.isTrue()) {
			return def;
		} else if(d instanceof LispCharacter) {
			return d.getCharacterCodePoint();
		} else {
			throw mesg.getError("err.require.char", d);
		}
	}

	/**
	 * 
	 * @param itr
	 * @param mesg
	 * @param body
	 * @return
	 */
	public static BigInteger nextInteger(
			Iterator<Datum> itr, LispMessage mesg, Datum body) {
		Datum d = Iterators.nextIf(itr, (Datum)null);

		if(d == null) {
			throw mesg.getError("err.argument", body);
		} else if(d instanceof LispInteger) {
			return d.getBigInteger();
		} else {
			throw mesg.getError("err.require.int", d);
		}
	}

	/**
	 * 
	 * @param ch
	 * @return
	 */
	public static int toFoldCase(int ch) {
		if(ch == 0x130 || ch == 0x131) {  // Turkish i
			return ch;
		}
		return Character.toLowerCase(Character.toUpperCase(ch));
	}

	/**
	 * 
	 * @param cs
	 * @param b
	 * @param e
	 * @return
	 */
	public static String toFoldCase(CharSequence cs, int b, int e) {
		StringBuilder bf = new StringBuilder();

		for(int i = b; i < e; i++) {
			bf.append((char)toFoldCase(cs.charAt(i)));
		}
		return bf.toString();
	}

	/**
	 * 
	 * @param cs
	 * @param b
	 * @param e
	 * @return
	 */
	public static String toFoldCase(CharSequence cs) {
		return toFoldCase(cs, 0, cs.length());
	}

	/**
	 * @param c1a
	 * @param mesg
	 * @return
	 */
	public static URI getURI(Datum x, LispMessage mesg) {
		if(x instanceof LispString) {
			try {
				return new URI(x.getString());
			} catch (URISyntaxException e) {
				throw mesg.getError("err.net.require.uri", x);
			}
		} else {
			throw mesg.getError("err.net.require.uri", x);
		}
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static Datum nvl(Datum x) {
		return (x != null) ? x : LispBoolean.FALSE;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static Datum nvl(String x) {
		return (x != null) ? new LispString(x) : LispBoolean.FALSE;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static Datum positive(int x) {
		return (x >= 0) ? LispInteger.valueOf(x) : LispBoolean.FALSE;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static Datum positive(long x) {
		return (x >= 0) ? LispInteger.valueOf(x) : LispBoolean.FALSE;
	}

}
