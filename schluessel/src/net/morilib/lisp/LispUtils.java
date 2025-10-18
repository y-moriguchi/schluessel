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

import java.lang.reflect.Array;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.Stack;

import net.morilib.lisp.array.ILispArray;
import net.morilib.lisp.datetime.LispDate;
import net.morilib.lisp.uvector.HomogeneousArray;
import net.morilib.util.ArrayListStack;
import net.morilib.util.Stack2;
import net.morilib.util.mapset.HashOneToOneSet;
import net.morilib.util.mapset.OneToOneSet;
import net.morilib.util.table.ArrayTable;
import net.morilib.util.table.Table;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class LispUtils {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/18
	 */
	public static interface CallTable<C, T> {

		/**
		 * 
		 * @param rows
		 * @param cols
		 * @return
		 */
		public C create(int rows, int cols);

		/**
		 * 
		 * @param r
		 * @param c
		 * @param t
		 */
		public void call(C tbl, int r, int c, T t);

	}

	//
	private LispUtils() {
		// do nothing
	}

	//
//	private static void printBodyAux1(
//			Cons c, StringBuilder buf, boolean met, String disp) {
//		if(c.getCdr() instanceof Cons) {
//			Cons cd = (Cons)c.getCdr();
//
//			if(cd.getCdr() == Nil.NIL) {
//				buf.append(disp);
//				printBody(cd.getCar(), buf, met);
//			}
//		}
//	}

	//
//	private static void printBody(
//			Datum d, StringBuilder buf, boolean met) {
//		if(d instanceof Atom) {
//			Atom a = (Atom)d;
//
//			buf.append((met ? a.getResult() : a.print()));
////		} else if(d == Nil.NIL) {
////			buf.append("()");
////		} else if(d == Undef.UNDEF) {
////			buf.append("#<undef>");
//		} else if(d instanceof Cons) {
//			Cons c = (Cons)d;
//
//			// quoteされた文字列のとき
//			if(Symbol.QUOTE.equals(c.getCar())) {
//				printBodyAux1(c, buf, met, "'");
//			} else if(Symbol.QUASIQUOTE.equals(c.getCar())) {
//				printBodyAux1(c, buf, met, "`");
//			} else if(Symbol.UNQUOTE.equals(c.getCar())) {
//				printBodyAux1(c, buf, met, ",");
//			} else if(Symbol.UNQUOTE_SPLICING.equals(c.getCar())) {
//				printBodyAux1(c, buf, met, ",@");
//			} else {
//				Set<Datum> jn = new HashSet<Datum>();
//
//				buf.append("(");
//				while(true) {
//					// car
//					printBody(c.getCar(), buf, met);
//
//					// cdr
//					if(c.getCdr() instanceof Cons) {
//						if(jn.contains(c)) {
//							// circular list
//							buf.append(" ...");
//							break;
//						}
//						jn.add(c);
//
//						buf.append(" ");
//						c = (Cons)c.getCdr();
//					} else if(c.getCdr() == Nil.NIL) {
//						break;
//					} else {
//						buf.append(" . ");
//						printBody(c.getCdr(), buf, met);
//						break;
//					}
//				}
//				buf.append(")");
//			}
//		} else if(d instanceof LispVector) {
//			LispVector v = (LispVector)d;
//
//			buf.append("#(");
//			for(int i = 0; i < v.size(); i++) {
//				if(i > 0) {
//					buf.append(" ");
//				}
//				printBody(v.get(i), buf, met);
//			}
//			buf.append(")");
////		} else if(d instanceof ClosureClass) {
////			buf.append("#<closureClass" + d + ">");
////		} else if(d instanceof Closure) {
////			buf.append("#<closure " + ((Closure)d).printName() + ">");
////		} else if(d instanceof Continuation) {
////			buf.append("#<continuation " +
////					Integer.toString(d.hashCode(), 16) + ">");
////		} else if(d instanceof Subr) {
////			buf.append("#<subr " + ((Subr)d).getSymbolName() + ">");
////		} else if(d instanceof Syntax) {
////			buf.append("#<syntax " + ((Syntax)d).getSymbolName() + ">");
////		} else if(d instanceof UserSyntax) {
////			buf.append("#<syntax " + ((UserSyntax)d).getName() + ">");
////		} else if(d instanceof Macro) {
////			buf.append("#<macro>");
////		} else if(d instanceof Promise) {
////			buf.append("#<promise " + d + ">");
////		} else if(d == EOFObject.EOF) {
////			buf.append("#<eof>");
////		} else if(d instanceof InputPort) {
////			buf.append("#<iport>");
////		} else if(d instanceof OutputPort) {
////			buf.append("#<oport>");
//		} else if(d instanceof MultiValues) {
//			List<Datum> lst = ((MultiValues)d).getValues();
//
//			for(int i = 0; i < lst.size(); i++) {
//				if(i > 0) {
//					buf.append("\n");
//				}
//				//buf.append(lst.get(i));
//				printBody(lst.get(i), buf, met);
//			}
////		} else if(d instanceof EnvironmentObject) {
////			buf.append("#<environment>");
////		} else if(d instanceof JavaClass) {
////			buf.append("#<java-class>");
////		} else if(d instanceof JavaInstance) {
////			Object o = ((JavaInstance)d).getJavaInstance();
////
////			buf.append("#<java-instance ");
////			buf.append(o.getClass().getName());
////			buf.append(">");
////		} else if(d == JavaNull.JAVA_NULL) {
////			buf.append("#<java-null>");
//		} else if(d instanceof SymbolScope) {
//			// 内部使用データ:シンボルと同等に扱う
//			SymbolScope d2 = (SymbolScope)d;
//			Symbol a = d2.getSymbol();
//
//			buf.append((met ? a.getResult() : a.print()));
//			//buf.append("#<symbolscope " +
//			//		d2.getSymbol().getName() + ">");
////		} else if(d instanceof RegexPattern) {
////			buf.append("#<regexp " +
////					((RegexPattern)d).getPatternString() + ">");
////		} else if(d instanceof RegexPattern.Match) {
////			buf.append("#<rxmatch " +
////					((RegexPattern.Match)d).toStringRepl() + ">");
////		} else if(d instanceof Keyword) {
////			buf.append(":" + ((Keyword)d).getName());
////		} else if(d instanceof NamableDatum) {
////			buf.append(((NamableDatum)d).display());
//		} else {
//			d.toDisplayString(buf);
//		}
//	}

	//
	private static class DumDatum extends Datum {

	}

	//
	private static final Datum _STK_CONS = new DumDatum();
	private static final Datum _STK_CNS2 = new DumDatum();
	private static final Datum _STK_VEC1 = new DumDatum();
	private static final Datum _STK_VEC2 = new DumDatum();
	private static final Datum _STK_MLT1 = new DumDatum();

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static boolean detectCirculate(Datum s) {
		Stack<Datum> st = new Stack<Datum>();
		Map<Datum, Integer> e1 = new IdentityHashMap<Datum, Integer>();
		Datum r;

		st.push(s);
		while(!st.isEmpty()) {
			r = st.pop();
			if(r instanceof Cons) {
				Cons c = (Cons)r;

				if(c.getCar() == s) {
					return true;
				} else if(e1.containsKey(c.getCar())) {
					// do nothing
				} else {
					st.push(c.getCar());
					e1.put(c.getCar(), 0);
				}

				if(c.getCdr() == s) {
					return true;
				} else if(e1.containsKey(c.getCdr())) {
					// do nothing
				} else {
					st.push(c.getCdr());
					e1.put(c.getCdr(), 0);
				}
			} else if(r instanceof LispVector) {
				LispVector v = (LispVector)r;

				for(int i = 0; i < ((LispVector)r).size(); i++) {
					if(v.get(i) == s) {
						return true;
					} else if(e1.containsKey(v.get(i))) {
						// do nothing
					}
					e1.put(v.get(i), 0);
				}
			}
		}
		return false;
	}

	//
	private static void printBody(
			Datum dt, StringBuilder buf, boolean met,
			Map<Datum, Object> eq, boolean s38) {
		Stack2<Datum> st = new ArrayListStack<Datum>();
//		Map<Datum, Object> eq = new IdentityHashMap<Datum, Object>();
		int num = 0;

		st.push(dt);
		while(!st.isEmpty()) {
			Datum d = st.pop();

			if(d instanceof ILispArray && !(d instanceof LispVector)) {
				ILispArray a = (ILispArray)d;
				int[] is = new int[a.rank()];

				buf.append('#');
				buf.append(a.rank());
				buf.append('A');

				if(a.rank() > 0) {
					for(int i = 0; i < a.rank(); i++) {
						if(i > 0) {
							buf.append('*');
						}
						buf.append(a.endIndex(i) - a.startIndex(i));
						is[i] = a.startIndex(i);
					}
					if(a.getTypeSpecifier() != null) {
						buf.append(':');
						buf.append(a.getTypeSpecifier());
					}
					_append(a, buf, eq, met, s38, 0, is);
				} else {
					if(a.getTypeSpecifier() != null) {
						buf.append(':');
						buf.append(a.getTypeSpecifier());
					}
					buf.append(' ');
					printBody(a.getFromArray(), buf, met, eq, s38);
				}
			} else if(d instanceof Atom) {
				Atom a = (Atom)d;

				buf.append((met ? a.getResult() : a.print()));
			} else if(s38 && eq.containsKey(d)) {
				buf.append("#" + eq.get(d) + "#");
			} else if(d instanceof Cons) {
				Cons c = (Cons)d;

				if(s38 && detectCirculate(d)) {
					buf.append("#" + num + "=");
					eq.put(d, num++);
				}

				if(Symbol.QUOTE.equals(c.getCar()) ||
						Symbol.QUASIQUOTE.equals(c.getCar()) ||
						Symbol.UNQUOTE.equals(c.getCar()) ||
						Symbol.UNQUOTE_SPLICING.equals(c.getCar())) {
					if(c.getCdr() instanceof Cons) {
						Cons cd = (Cons)c.getCdr();

						if(cd.getCdr() == Nil.NIL) {
							if(Symbol.QUOTE.equals(c.getCar())) {
								buf.append("'");
							} else if(Symbol.QUASIQUOTE.equals(
									c.getCar())) {
								buf.append("`");
							} else if(Symbol.UNQUOTE.equals(
									c.getCar())) {
								buf.append(",");
							} else if(Symbol.UNQUOTE_SPLICING.equals(
									c.getCar())) {
								buf.append(",@");
							}
							st.push(cd.getCar());
							continue;
						}
					}
				}
				buf.append("(");
				st.push(c.getCdr());
				st.push(_STK_CONS);
				st.push(c.getCar());
			} else if(d instanceof LispVector) {
				LispVector v = (LispVector)d;

				if(s38 && detectCirculate(d)) {
					buf.append("#" + num + "=");
					eq.put(d, num++);
				}

				buf.append("#(");
				st.push(_STK_VEC2);
				for(int i = v.size() - 1; i >= 0; i--) {
					st.push(v.get(i));
					if(i > 0) {
						st.push(_STK_VEC1);
					}
				}
			} else if(d instanceof MultiValues) {
				List<Datum> lst = ((MultiValues)d).getValues();

				for(int i = lst.size() - 1; i >= 0; i--) {
					st.push(lst.get(i));
					if(i > 0) {
						st.push(_STK_MLT1);
					}
				}
			} else if(d instanceof SymbolScope) {
				// 内部使用データ:シンボルと同等に扱う
				SymbolScope d2 = (SymbolScope)d;
				Symbol a = d2.getSymbol();

				buf.append((met ? a.getResult() : a.print()));
			} else if(d == _STK_CONS) {
				Datum e = st.pop();

				if(s38 && eq.containsKey(e)) {
					buf.append(" . #" + eq.get(e) + "#)");
				} else if(e instanceof Cons) {
					buf.append(" ");
					st.push(((Cons)e).getCdr());
					st.push(_STK_CONS);
					st.push(((Cons)e).getCar());
				} else if(e.isNil()) {
					buf.append(")");
				} else {
					buf.append(" . ");
					st.push(_STK_CNS2);
					st.push(e);
				}
			} else if(d == _STK_CNS2) {
				buf.append(")");
			} else if(d == _STK_VEC1) {
				buf.append(" ");
			} else if(d == _STK_VEC2) {
				buf.append(")");
			} else if(d == _STK_MLT1) {
				buf.append("\n");
			} else if(d instanceof Datum3) {
				if(met) {
					((Datum3)d).getResult(buf);
				} else {
					((Datum3)d).print(buf);
				}
			} else {
				d.toDisplayString(buf);
			}
		}
	}

	//
	private static void _append(ILispArray a, StringBuilder b,
			Map<Datum, Object> eq, boolean met, boolean s38, int dp,
			int[] is) {
		if(dp == is.length) {
			printBody(a.getFromArray(is), b, met, s38);
		} else {
			b.append("(");
			for(; is[dp] < a.endIndex(dp); is[dp]++) {
				if(is[dp] > a.startIndex(dp)) {
					b.append(" ");
				}
				_append(a, b, eq, met, s38, dp + 1, is);
			}
			b.append(")");
			is[dp] = a.startIndex(dp);
		}
	}

	/**
	 * @param d
	 * @param buf
	 * @param b
	 * @param c
	 */
	private static void printBody(Datum d, StringBuilder buf,
			boolean b, boolean c) {
		printBody(d, buf, b, new IdentityHashMap<Datum, Object>(), c);
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static String print(Datum d) {
		StringBuilder buf = new StringBuilder();

		printBody(d, buf, false, true);
		return buf.toString();
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static String getResult(Datum d) {
		StringBuilder buf = new StringBuilder();

		printBody(d, buf, true, true);
		return buf.toString();
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static String printWithoutSS(Datum d) {
		StringBuilder buf = new StringBuilder();

		printBody(d, buf, false, false);
		return buf.toString();
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static String getResultWithoutSS(Datum d) {
		StringBuilder buf = new StringBuilder();

		printBody(d, buf, true, false);
		return buf.toString();
	}

	/**
	 * 
	 * @param d
	 * @param dot
	 * @return
	 */
	public static Datum listToCons(
			Collection<? extends Datum> d, Datum dot) {
		Cons res = null;
		Cons c2 = null;

//		for(int i = 0; i < d.size(); i++) {
		for(Datum x : d) {
			if(res == null) {
				res = c2 = new Cons();
//				c2.setCar(d.get(i));
				c2.setCar(x);
			} else {
//				Cons c3 = new Cons(d.get(i), Nil.NIL);
				Cons c3 = new Cons(x, Nil.NIL);
				c2.setCdr(c3);
				c2 = c3;
			}
		}

		if(res == null) {
			return dot;
		} else {
			c2.setCdr(dot);
			return res;
		}
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static Datum listToCons(Collection<? extends Datum> d) {
		return listToCons(d, Nil.NIL);
	}

	/**
	 * 
	 * @param d
	 * @param dot
	 * @return
	 */
	public static Datum toCons(Iterable<? extends Datum> d,
			Datum dot) {
		Cons res = null;
		Cons c2 = null;

		for(Datum x : d) {
			if(res == null) {
				res = c2 = new Cons();
				c2.setCar(x);
			} else {
				Cons c3 = new Cons(x, Nil.NIL);
				c2.setCdr(c3);
				c2 = c3;
			}
		}

		if(res == null) {
			return dot;
		} else {
			c2.setCdr(dot);
			return res;
		}
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static Datum toCons(Iterable<? extends Datum> d) {
		return toCons(d, Nil.NIL);
	}

	/**
	 * 
	 * @param d
	 * @param mesg
	 * @return
	 */
	public static List<Datum> consToList(Datum d, LispMessage mesg) {
		List<Datum> res = new ArrayList<Datum>();
		Datum dd = d;

		while(dd != Nil.NIL) {
			if(dd instanceof Cons) {
				res.add(((Cons)dd).getCar());
				dd = ((Cons)dd).getCdr();
			} else {
				//throw new NotProperException("proper list required");
				throw mesg.getError("err.list");
			}
		}
		return res;
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static List<Datum> consToListIgnoreDot(Datum d) {
		List<Datum> res = new ArrayList<Datum>();
		Datum dd = d;

		while(dd != Nil.NIL) {
			if(dd instanceof Cons) {
				res.add(((Cons)dd).getCar());
				dd = ((Cons)dd).getCdr();
			} else {
				break;
			}
		}
		return res;
	}

	/**
	 * 
	 * @param d
	 * @param mesg
	 * @return
	 */
	public static Datum[] consToArray(Datum d, LispMessage mesg) {
		return consToList(d, mesg).toArray(new Datum[0]);
	}

	/**
	 * 
	 * @param d
	 * @param mesg
	 * @return
	 */
	public static<C> C consToTable(
			Datum d, LispMessage mesg,
			CallTable<C, Datum> call) {
		ConsIterator ity;
		int c = -1, r = 0;
		C res;

		ity = new ConsIterator(d);
		while(ity.hasNext()) {
			Datum d2;
			ConsIterator itx;
			int x2 = 0;

			if(!((d2 = ity.next()) instanceof Cons)) {
				throw mesg.getError("err.require.list", d2);
			}
			itx = new ConsIterator(d2);
			while(itx.hasNext()) {
				itx.next();
				x2++;
			}

			if(!itx.getTerminal().isNil()) {
				throw mesg.getError("err.list", d2);
			} else if(c < 0) {
				c = x2;
			} else if(c != x2) {
				throw mesg.getError("err.table.malform.column", d2);
			}
			r++;
		}

		if(!ity.getTerminal().isNil()) {
			throw mesg.getError("err.list", d);
		}

		res = call.create(r, c);
		ity = new ConsIterator(d);
		r = 0;
		while(ity.hasNext()) {
			ConsIterator itx;

			itx = new ConsIterator(ity.next());
			c = 0;
			while(itx.hasNext()) {
				call.call(res, r, c, itx.next());
				c++;
			}
			r++;
		}
		return res;
	}

	//
	private static final CallTable<Table<Datum>, Datum>
	_MAKETBL = new CallTable<Table<Datum>, Datum>() {

		public Table<Datum> create(int rows, int cols) {
			return new ArrayTable<Datum>(rows, cols);
		}

		public void call(Table<Datum> tbl, int r, int c, Datum t) {
			tbl.set(r, c, t);
		}

	};

	/**
	 * 
	 * @param d
	 * @param mesg
	 * @return
	 */
	public static Table<Datum> consToTable(Datum d, LispMessage mesg) {
		return consToTable(d, mesg, _MAKETBL);
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static int consLength(Datum d) {
		Datum p = d;
		int len = 0;

		while(p != Nil.NIL) {
			if(p instanceof Cons) {
				Cons c = (Cons)p;

				len++;
				p = c.getCdr();
			} else {
				return len;
			}
		}
		return len;
	}

	/**
	 * 
	 * @param arr
	 * @return
	 */
	public static List<Datum> toList(Object[] arr) {
		List<Datum> lst = new ArrayList<Datum>();

		for(Object o : arr) {
			lst.add(toDatum(o));
		}
		return lst;
	}

	/**
	 * 
	 * @param arr
	 * @param cdr
	 * @return
	 */
	public static Datum toConsList(Object[] arr, Object cdr) {
		ConsListBuilder lst = new ConsListBuilder();

		for(Object o : arr) {
			lst.append(toDatum(o));
		}
		return lst.get(toDatum(cdr));
	}

	/**
	 * 
	 * @param arr
	 * @return
	 */
	public static Datum toConsList(Object[] arr) {
		return toConsList(arr, Nil.NIL);
	}

	/**
	 * 
	 * @param v
	 * @return
	 */
	public static LispExactReal bigDecimalToRational(
			BigDecimal v) {
		BigInteger n = v.unscaledValue();

		if(v.scale() > 0) {
			BigInteger d = BigInteger.TEN.pow(v.scale());

			return LispRational.newRational(n, d);
		} else if(v.scale() < 0) {
			BigInteger d = BigInteger.TEN.pow(-v.scale());

			return LispInteger.valueOf(n.multiply(d));
		} else {
			return LispInteger.valueOf(n);
		}
	}

	/**
	 * 
	 * @param o
	 * @return
	 */
	public static Datum toDatum(Object o) {
		Class<?> cl = (o == null) ? null : o.getClass();

		if(o == null) {
			return JavaNull.JAVA_NULL;
		} if(o instanceof Datum) {
			return (Datum)o;
		} else if(o instanceof Byte) {
			return LispInteger.valueOf(((Byte)o).intValue());
		} else if(o instanceof Short) {
			return LispInteger.valueOf(((Short)o).intValue());
		} else if(o instanceof Integer) {
			return LispInteger.valueOf(((Integer)o).intValue());
		} else if(o instanceof Long) {
			return LispInteger.valueOf(((Long)o).longValue());
		} else if(o instanceof BigInteger) {
			return LispInteger.valueOf((BigInteger)o);
		} else if(o instanceof BigDecimal) {
			return bigDecimalToRational((BigDecimal)o);
		} else if(o instanceof Float) {
			return new LispDouble(((Float)o).doubleValue());
		} else if(o instanceof Double) {
			return new LispDouble(((Double)o).doubleValue());
		} else if(o instanceof String) {
			return new LispString((String)o);
		} else if(o instanceof Character) {
			return new LispCharacter(((Character)o).charValue());
		} else if(o instanceof Boolean) {
			return LispBoolean.getInstance(((Boolean)o).booleanValue());
		} else if(cl.isArray()) {
			ConsListBuilder bld = new ConsListBuilder();
			int len = Array.getLength(o);

			for(int i = 0; i < len; i++) {
				bld.append(toDatum(Array.get(o, i)));
			}
			return bld.get();
		} else {
			return new JavaInstance(o);
			//throw new ClassCastException();
		}
	}

	/**
	 * 
	 * @param d1
	 * @param d2
	 * @return
	 */
	public static boolean eqv(Datum d1, Datum d2) {
		return !LispBoolean.FALSE.equals(d1.isEqv(d2));
	}

	/**
	 * 
	 * @param d
	 * @param o
	 * @return
	 */
	public static boolean equals(Datum d, String o) {
		if(d instanceof LispString) {
			return ((LispString)d).getString().equals((String)o);
		} else {
			return false;
		}
	}

	/**
	 * 
	 * @param d
	 * @param v
	 * @return
	 */
	public static boolean eqvExact(Datum d, BigInteger v) {
		if(d instanceof LispInteger) {
			return d.getBigInteger().equals(v);
		} else {
			return false;
		}
	}

	/**
	 * 
	 * @param d
	 * @param v
	 * @return
	 */
	public static boolean eqvExact(Datum d, int v) {
		return eqvExact(d, BigInteger.valueOf(v));
	}

	/**
	 * 
	 * @param d
	 * @param v
	 * @return
	 */
	public static boolean eqvExact(Datum d, long v) {
		return eqvExact(d, BigInteger.valueOf(v));
	}

	/**
	 * 
	 * @param d
	 * @param v
	 * @return
	 */
	public static boolean eqvInexact(Datum d, double v) {
		if(d instanceof LispReal) {
			return ((LispReal)d).getRealDouble() == v;
		} else {
			return false;
		}
	}

	/**
	 * 
	 * @param d
	 * @param v
	 * @return
	 */
	public static boolean eqvInexact(Datum d, Number v) {
		return eqvInexact(d, v.doubleValue());
	}

	/**
	 * 
	 * @param d
	 * @param v
	 * @return
	 */
	public static boolean eqvInexact(Datum d, int v) {
		return eqvInexact(d, (double)v);
	}

	/**
	 * 
	 * @param d
	 * @param v
	 * @return
	 */
	public static boolean eqvInexact(Datum d, long v) {
		return eqvInexact(d, (double)v);
	}

	/**
	 * 
	 * @param d
	 * @param lst
	 * @return
	 */
	public static Datum listDot(Object d, Object... lst) {
		ConsListBuilder b = new ConsListBuilder();

		for(Object o : lst) {
			b.append(toDatum(o));
		}
		return b.get(toDatum(d));
	}

	/**
	 * 
	 * @param lst
	 * @return
	 */
	public static Datum list(Object... lst) {
		return listDot(Nil.NIL, lst);
	}

	/**
	 * 
	 * @param d
	 * @param lst
	 * @return
	 */
	public static Datum listDot(Datum d, Datum... lst) {
		ConsListBuilder b = new ConsListBuilder();

		for(Datum o : lst) {
			b.append(o);
		}
		return b.get(d);
	}

	/**
	 * 
	 * @param lst
	 * @return
	 */
	public static Datum list(Datum... lst) {
		return listDot(Nil.NIL, lst);
	}

	/**
	 * 
	 * @param car
	 * @param cdr
	 * @return
	 */
	public static Cons cons(Object car, Object cdr) {
		return new Cons(toDatum(car), toDatum(cdr));
	}

	/**
	 * 
	 * @param lst
	 * @return
	 */
	public static LispVector vector(Object... lst) {
		List<Datum> v = new ArrayList<Datum>();

		for(Object o : lst) {
			v.add(toDatum(o));
		}
		return new LispVector(v);
	}

	/**
	 * 
	 * @param v1
	 * @param v2
	 * @return
	 */
	public static boolean equalsVector(LispVector v1, LispVector v2) {
		if(v1.size() != v2.size()) {
			return false;
		} else {
			for(int i = 0; i < v1.size(); i++) {
				if(!equal(v1.get(i), v2.get(i))) {
					return false;
				}
			}
			return true;
		}
	}

	/**
	 * 
	 * @param d1
	 * @param d2
	 * @return
	 */
	/*package*/ static boolean equal(Datum d1, Datum d2) {
		Datum p = d1;
		Datum q = d2;

		while(true) {
			if(p instanceof Cons) {
				Cons pc = (Cons)p;

				if(q instanceof Cons) {
					Cons qc = (Cons)q;

					if(equal(pc.getCar(), qc.getCar())) {
						p = pc.getCdr();
						q = qc.getCdr();
					} else {
						return false;
					}
				} else {
					return false;
				}
			} else if(p instanceof LispString) {
				if(q instanceof LispString) {
					String s1 = ((LispString)p).getString();
					String s2 = ((LispString)q).getString();

					return s1.equals(s2);
				} else {
					return false;
				}
			} else if(p instanceof LispVector) {
				if(q instanceof LispVector) {
					return equalsVector((LispVector)p, (LispVector)q);
				} else {
					return false;
				}
			} else if(p instanceof HomogeneousArray) {
				return ((q instanceof HomogeneousArray) &&
						((HomogeneousArray)p).equalsArray(p, q));
			} else {
				return p.isEqv(q); 
			}
		}
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static boolean equals(Datum a, Datum b) {
		Stack2<Datum> s1 = new ArrayListStack<Datum>();
		Stack2<Datum> s2 = new ArrayListStack<Datum>();
		Map<Datum, Integer> e1 = new IdentityHashMap<Datum, Integer>();
		Map<Datum, Integer> e2 = new IdentityHashMap<Datum, Integer>();
		int num = 0;

		s1.push(a);
		s2.push(b);
		while(!s1.isEmpty()) {
			Datum x = s1.pop();
			Datum y = s2.pop();

			if(x == y) {
				// do nothing
			} else if(e1.containsKey(x) && e2.containsKey(y) &&
					e1.get(x) == e2.get(y)) {
				// do nothing
			} else if(x instanceof Cons && y instanceof Cons) {
				s1.push(((Cons)x).getCdr());
				s1.push(((Cons)x).getCar());
				s2.push(((Cons)y).getCdr());
				s2.push(((Cons)y).getCar());
			} else if(x instanceof LispVector &&
					y instanceof LispVector) {
				LispVector xv = (LispVector)x;
				LispVector yv = (LispVector)y;

				if(xv.size() != yv.size()) {
					return false;
				} else {
					for(int i = xv.size() - 1; i >= 0; i--) {
						s1.push(xv.get(i));
						s2.push(yv.get(i));
					}
				}
			} else if(x instanceof LispString &&
					y instanceof LispString) {
				if(!x.getString().equals(y.getString())) {
					return false;
				}
			} else if(x instanceof HomogeneousArray) {
				return ((y instanceof HomogeneousArray) &&
						((HomogeneousArray)x).equalsArray(x, y));
			} else if(x instanceof ILispArray) {
				return ((y instanceof ILispArray) &&
						((ILispArray)x).isEqualTo((ILispArray)y));
			} else if(!x.equals(y)) {
				return false;
			}

			e1.put(x, num);
			e2.put(y, num);
			num++;
		}
		return true;
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static Map<Datum, Datum> assocToMap(Datum d) {
		Map<Datum, Datum> res = new HashMap<Datum, Datum>();
		ConsIterator p = new ConsIterator(d);

		while(p.hasNext()) {
			Datum c = p.next();

			if(c instanceof Cons) {
				Cons c0 = (Cons)c;

				res.put(c0.getCar(), c0.getCdr());
			} else {
				return null;
			}
		}

		return (p.getTerminal() == Nil.NIL) ? res : null;
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static Map<Symbol, Datum> assocToMapSymbol(Datum d) {
		Map<Symbol, Datum> res = new HashMap<Symbol, Datum>();
		ConsIterator p = new ConsIterator(d);

		while(p.hasNext()) {
			Datum c = p.next();

			if(c instanceof Cons) {
				Cons c0 = (Cons)c;

				if(c0.getCar() instanceof SymbolName) {
					res.put(((SymbolName)c0.getCar()).getSymbol(),
							c0.getCdr());
				} else {
					return null;
				}
			} else {
				return null;
			}
		}

		return (p.getTerminal() == Nil.NIL) ? res : null;
	}

	/**
	 * 
	 * @param d
	 * @param mesg
	 */
	public static void checkReal(Datum d, LispMessage mesg) {
		if(!(d instanceof LispNumber)) {
			throw mesg.getError("err.require.number", d);
		} else if(!((LispNumber)d).isReal()) {
			throw mesg.getError("err.require.real", d);
		}
	}

	/**
	 * 
	 * @param l
	 * @param i
	 * @param mesg
	 */
	public static void checkReal(
			List<Datum> l, int i, LispMessage mesg) {
		checkReal(l.get(i), mesg);
	}

	/**
	 * 
	 * @param d
	 * @param mesg
	 */
	public static void checkString(Datum d, LispMessage mesg) {
		if(!(d instanceof LispString)) {
			throw mesg.getError("err.require.string", d);
		}
	}

	/**
	 * 
	 * @param d
	 * @param mesg
	 */
	public static void checkSymbol(Datum d, LispMessage mesg) {
		if(!(d instanceof Symbol)) {
			throw mesg.getError("err.require.symbol", d);
		}
	}

	/**
	 * @param body
	 */
	public static Datum[] toArray(Datum d, LispMessage mesg) {
		return consToList(d, mesg).toArray(new Datum[0]);
	}

	/**
	 * 
	 * @param itr
	 * @return
	 */
	public static Cons copy(Iterator<Datum> itr, Datum d) {
		Cons r = null, r0 = null;

		while(itr.hasNext()) {
			if(r == null) {
				r = r0 = new Cons();
			} else {
				r.setCdr(new Cons());
				r = (Cons)r.getCdr();
			}
			r.setCar(itr.next());
		}

		if(r0 != null) {
			r.setCdr(d);
		}
		return r0;
	}

	/**
	 * 
	 * @param itr
	 * @return
	 */
	public static Set<Datum> toSetEqv(Iterator<Datum> itr) {
		Set<Datum> d = new HashSet<Datum>();

		while(itr.hasNext()) {
			d.add(itr.next());
		}
		return d;
	}

	/**
	 * 
	 * @param itr
	 * @return
	 */
	public static List<Datum> toList(Iterator<Datum> itr) {
		List<Datum> d = new ArrayList<Datum>();

		while(itr.hasNext()) {
			d.add(itr.next());
		}
		return d;
	}

	/**
	 * 
	 * @param itr
	 * @return
	 */
	public static Datum toAlist(Iterator<Entry<Datum, Datum>> itr) {
		ConsListBuilder b = new ConsListBuilder();

		while(itr.hasNext()) {
			Entry<Datum, Datum> e = itr.next();

			b.append(new Cons(e.getKey(), e.getValue()));
		}
		return b.get();
	}

	/**
	 * 
	 * @param m
	 * @return
	 */
	public static Datum toAlist(
			Map<? extends Datum, ? extends Datum> m) {
		ConsListBuilder b = new ConsListBuilder();

		for(Map.Entry<? extends Datum, ? extends Datum> e : m.entrySet()) {
			b.append(new Cons(e.getKey(), e.getValue()));
		}
		return b.get();
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static Cons nconc(Cons a, Datum b) {
		Cons p = (Cons)a;
	
		while(true) {
			if(p.getCdr() instanceof Cons) {
				p = (Cons)p.getCdr();
			} else {
				break;
			}
		}
		p.setCdr(b);
		return a;
	}

	/**
	 * @param lispString
	 * @return
	 */
	public static Datum stringToList(CharSequence cs) {
		ConsListBuilder b = new ConsListBuilder();

		for(int i = 0; i < cs.length(); i++) {
			b.append(LispCharacter.valueOf(cs.charAt(i)));
		}
		return b.get();
	}

	/**
	 * 
	 * @param p
	 * @param x
	 * @param proc
	 * @param env
	 * @param mesg
	 * @return
	 */
	public static boolean contains(SExpression p, Datum x,
			Procedure proc, Environment env, LispMessage mesg) {
		for(Datum y : p) {
			if(Scheme.callva(proc, env, mesg, x, y).isTrue()) {
				return true;
			}
		}
		return false;
	}

	/**
	 * 
	 * @param p
	 * @param q
	 * @param proc
	 * @param env
	 * @param mesg
	 * @return
	 */
	public static boolean containsAsSet(SExpressionDatum p,
			SExpressionDatum q, Procedure proc, Environment env,
			LispMessage mesg) {
		for(Datum x : q) {
			if(!contains(p, x, proc, env, mesg)) {
				return false;
			}
		}
		return true;
	}

	/**
	 * 
	 * @param dd
	 * @return
	 */
	public static long length(Datum dd) {
		Map<Datum, Datum> eq = new IdentityHashMap<Datum, Datum>();
		long l = 0;

		while(!dd.isNil()) {
			if(eq.containsKey(dd)) {
				return -1;
			} else if(dd instanceof Cons) {
				l++;
				eq.put(dd, dd);
				dd = ((Cons)dd).getCdr();
			} else {
				return -2;
			}
		}
		return l;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static boolean isProperList(Datum x) {
		return length(x) >= 0;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static boolean isProperListNotNil(Datum x) {
		return length(x) > 0;
	}

	/**
	 * 
	 * @param x
	 * @param n
	 * @param mesg
	 * @return
	 */
	public static Datum take(Datum x, int n) {
		Datum x1 = x;
		Cons  r0, rp;

		r0 = rp = null;
		for(int i = 0; i < n; i++) {
			if(x1 instanceof Cons) {
				if(r0 == null) {
					rp = new Cons();
					r0 = rp;
				} else {
					rp.setCdr(new Cons());
					rp = (Cons)rp.getCdr();
				}
				rp.setCar(((Cons)x1).getCar());
				rp.setCdr(Nil.NIL);
				x1 = ((Cons)x1).getCdr();
			} else {
				if(r0 == null) {
					return x1;
				} else {
					rp.setCdr(x1);
					return r0;
				}
			}
		}
		return (r0 != null) ? r0 : Nil.NIL;
	}

	/**
	 * 
	 * @param x
	 * @param y
	 * @return
	 */
	public static Datum mul(Datum x, Datum y) {
		return (x.isNil() || y.isNil()) ? Nil.NIL : new Cons(x, y);
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static boolean isProduct(Datum x) {
		Stack2<Datum> s = new ArrayListStack<Datum>();
		Set<Datum>    t = new HashSet<Datum>();
		Datum d;

		s.push(x);
		while(s.isEmpty()) {
			d = s.pop();
			if(t.contains(d)) {
				// has common reference
				return false;
			} else if(d instanceof Cons) {
				s.push(((Cons)d).getCdr());
				s.push(((Cons)d).getCar());
				t.add(d);
			} else if(d.isNil()) {
				return false;
			}
		}
		return true;
	}

	/**
	 * 
	 * @param x
	 * @param y
	 * @return
	 */
	public static Datum append(Datum x, Datum y) {
		ConsListBuilder b = new ConsListBuilder();
		ConsIterator itr = new ConsIterator(x);

		if(!(x instanceof Cons) || !(y instanceof Cons)) {
			return null;
		} else if(x.isNilUnit()) {
			return y;
		} else if(y.isNilUnit()) {
			return x;
		} else {
			while(itr.hasNext()) {
				b.append(itr.next());
			}
			return itr.getTerminal().isNil() ? b.get(y) : null;
		}
	}

	/**
	 * 
	 * @param x
	 * @param y
	 * @return
	 */
	public static Datum add(Datum x, Datum y) {
		Datum lx, ly;

		if(x.isNil()) {
			return y;
		} else if(y.isNil()) {
			return x;
		}
		lx = isProperList(x) ? x : list(x);
		ly = isProperList(y) ? y : list(y);
		return append(lx, ly);
	}

	/**
	 * 
	 * @param x
	 * @param y
	 * @return
	 */
	public static Procedure mul(final Procedure x, final Procedure y) {
		return new Subr("synthesized procedure") {

			@Override
			public Datum eval(Datum body, Environment env,
					LispMessage mesg) {
				Datum ry = Scheme.call(y, env, mesg, body);
				Datum[] ay;

				ay = ry.getValues().toArray(new Datum[0]);
				return Scheme.callva(x, env, mesg, ay);
			}

		};
	}

	//
	private static class LabeledCons extends Datum {

		//
		private long  label;
		//private Datum cons;

		//
		private LabeledCons(Datum c, long l) {
			this.label = l;
			//this.cons  = c;
		}

		//
		public int hashCode() {
			return (int)label;
		}

		//
		public boolean equals(Object o) {
			if(o instanceof LabeledCons) {
				return label == ((LabeledCons)o).label;
			}
			return false;
		}
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static List<Datum> serializeCons(Datum d) {
		List<Datum> r;
		Stack2<Datum> s1;
		OneToOneSet<Long, Datum> sc;
		long l = 0, m;
		Datum x;

		r  = new ArrayList<Datum>();
		s1 = new ArrayListStack<Datum>();
		sc = new HashOneToOneSet<Long, Datum>();
		s1.add(d);
		while(!s1.isEmpty()) {
			x = s1.pop();
			if(sc.containsValue(x)) {
				m = sc.getKey(x);
				r.add(new LabeledCons(x, l - m));
			} else {
				if(x instanceof Cons) {
					sc.put(l++, x);
					r.add(new LabeledCons(x, -1));
					s1.push(((Cons)x).getCdr());
					s1.push(((Cons)x).getCar());
				} else {
					r.add(x);
				}
			}
		}
		return r;
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static boolean containsTopologically(Datum a, Datum b) {
		List<Datum> a1, b1;

		a1 = serializeCons(a);
		b1 = serializeCons(b);
		outer: for(int i = 0; i <= a1.size() - b1.size(); i++) {
			for(int j = i; j < b1.size(); j++) {
				if(!a1.get(j).equals(b1.get(j))) {
					continue outer;
				}
			}
			return true;
		}
		return false;
	}

	/**
	 * 
	 * @param l
	 * @param d
	 * @return
	 */
	public static boolean contains(Datum l, Datum d) {
		Stack2<Datum> s1 = new ArrayListStack<Datum>();
		Set<Datum> sc = new HashSet<Datum>();
		Datum x;

		s1.add(l);
		while(!s1.isEmpty()) {
			x = s1.pop();
			if(x.equals(d)) {
				return true;
			} else if(sc.contains(x)) {
				// do nothing
			} else if(x instanceof Cons) {
				sc.add(x);
				s1.push(((Cons)x).getCdr());
				s1.push(((Cons)x).getCar());
			}
		}
		return false;
	}

	/**
	 * 
	 * @param l
	 * @param d
	 * @return
	 */
	public static int countLeaves(Datum d) {
		Stack2<Datum> s1 = new ArrayListStack<Datum>();
		Set<Datum> sc = new HashSet<Datum>();
		Datum x;
		int l = 0;

		s1.add(d);
		while(!s1.isEmpty()) {
			x = s1.pop();
			if(sc.contains(x)) {
				return -1;
			} else if(x instanceof Cons) {
				sc.add(x);
				s1.push(((Cons)x).getCdr());
				s1.push(((Cons)x).getCar());
			} else if(!x.isNil()) {
				l++;
			}
		}
		return l;
	}

	/**
	 * 
	 * @param d
	 * @param mesg
	 * @return
	 */
	public static Object toObject(Datum d, LispMessage mesg) {
		if(d instanceof JavaObjective) {
			return ((JavaObjective)d).toObject();
		} else {
			throw mesg.getError("err.require.java-instance", d);
		}
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static Datum toLispString(String s) {
		return (s != null) ? new LispString(s) : LispBoolean.FALSE;
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static Datum toPositiveInt(int x) {
		return (x >= 0) ? LispInteger.valueOf(x) : LispBoolean.FALSE;
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static Datum toDate(long x) {
		return (x != 0) ? new LispDate(x) : LispBoolean.FALSE;
	}

	/**
	 * @param charAt
	 * @return
	 */
	public static boolean isReservedCharacter(char c) {
		return (c == '(' || c == '[' ||
				c == ')' || c == ']' || c == '#' ||
				Character.isWhitespace(c));
	}

	/**
	 * 
	 * @param number
	 * @return
	 */
	public static BigInteger toIntegerExact(double number) {
		try {
			BigDecimal dec;
	
			if(Double.isInfinite(number) || Double.isNaN(number)) {
				return null;
			}
			dec = new BigDecimal(number);
			return dec.toBigIntegerExact();
		} catch(ArithmeticException e) {
			return null;
		}
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

}
