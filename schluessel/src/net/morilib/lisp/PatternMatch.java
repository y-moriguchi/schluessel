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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.morilib.lisp.exlib.Gensym;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.NullSet;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/24
 */
/*package*/ class PatternMatch {

	//
	private static final Datum ELLIPSIS = Symbol.getSymbol("...");
	private static final Datum UNDERBAR = Symbol.getSymbol("_");
	private static final int PATTERN = 0;
	private static final int TEMPLATE = 1;

	/*// 戻り値とパラメータ使用フラグを両方管理するためのデータ格納クラス
	private static class D2 {
		private Datum d;          // 戻り値となるデータ
		private boolean useprm;   // パラメータが使用されたときはtrue

		private D2() {
			// default
			useprm = false;
		}

		private D2(Datum d, boolean useprm) {
			this.d = d;
			this.useprm = useprm;
		}

	}*/

	/*private static class PrWrap extends Datum {

		private Datum wrapee;

		private PrWrap(Datum w) {
			wrapee = w;
		}

		public Datum getWrapee() {
			return wrapee;
		}

		public String toString() {
			return "p(" + wrapee + ")";
		}
	}*/

	//
	private static class LtWrap extends Datum {

		private Datum wrapee;

		private LtWrap(Datum w) {
			wrapee = w;
		}

		public Datum getWrapee() {
			return wrapee;
		}

		public String toString() {
			return "l(" + wrapee + ")";
		}
	}

	//
	private static class GtWrap extends Datum
	implements SyntaxUtils.SafeWrap {

		private Datum wrapee;

		private GtWrap(Datum w) {
			wrapee = w;
		}

		public Datum getWrapee() {
			return wrapee;
		}

		public String toString() {
			return "g(" + wrapee + ")";
		}
	}

	//
	private static class IsSynCase extends UnaryArgs {

		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof SymbolName) {
				Symbol s = ((SymbolName)c1a).getSymbol();
				//Datum  d = env.findDatum(s);

				//if(d instanceof SynSyntaxCase) {
				//	return LispBoolean.TRUE;
				//}
				return LispBoolean.getInstance(
						SynSyntaxCase.SYNTAX_CASE.equals(s));
			}
			return LispBoolean.FALSE;
		}

	}

	//
	private static class IsSynQuot extends UnaryArgs {

		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof SymbolName) {
				Symbol s = ((SymbolName)c1a).getSymbol();
				//Datum  d = env.findDatum(s);

				//if(d instanceof SynSyntax) {
				//	return LispBoolean.TRUE;
				//}
				return LispBoolean.getInstance(
						SynSyntax.SYNTAX.equals(s));
			}
			return LispBoolean.FALSE;
		}

	}

	//
	private static class IsSynQQuo extends UnaryArgs {

		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof SymbolName) {
				Symbol s = ((SymbolName)c1a).getSymbol();
				//Datum  d = env.findDatum(s);

				//if(d instanceof SynQuasisyntax) {
				//	return LispBoolean.TRUE;
				//}
				return LispBoolean.getInstance(
						SynQuasisyntax.QUASISYNTAX.equals(s));
			}
			return LispBoolean.FALSE;
		}

	}

	//
	private static class IsWithSyn extends UnaryArgs {

		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof SymbolName) {
				Symbol s = ((SymbolName)c1a).getSymbol();
				//Datum  d = env.findDatum(s);

				//if(d instanceof UserSyntax) {
				//	UserSyntax u2 = (UserSyntax)d;
				//	
				//	return LispBoolean.getInstance(
				//			"with-syntax".equals(u2.getName()));
				//}
				return LispBoolean.getInstance(
						SynSyntax.WITH_SYNTAX.equals(s));
			}
			return LispBoolean.FALSE;
		}

	}

	//
	private static class AddUSyn extends BinaryArgs {

		@Override
		protected Datum execute(
				Datum c1a, Datum c2a,
				Environment env, LispMessage mesg) {
			if(!(c1a instanceof Symbol)) {
				throw mesg.getError("err.require.symbol");
			} else if (!(c2a instanceof UserSyntax)) {
				throw mesg.getError("err.require.usersyntax");
			} else {
				return new SymbolScope(
						(Symbol)c1a, (UserSyntax)c2a);
			}

		}

	}

	//
	private static class AddLtWrap extends UnaryArgs {

		@Override
		protected Datum execute(
				Datum c1a,
				Environment env, LispMessage mesg) {
			return (c1a instanceof LtWrap) ? c1a : new LtWrap(c1a);
			//return new ExWrap(c1a);
		}

	}

	//
	private static class EllipsisC extends Subr {

		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			return ELLIPSIS;
		}

	}

	//
	private static final IsSynCase IS_SYN_CASE = new IsSynCase();
	private static final IsSynQuot IS_SYN_QUOT = new IsSynQuot();
	private static final IsWithSyn IS_WITH_SYN = new IsWithSyn();
	private static final IsSynQQuo IS_SYN_QQUT = new IsSynQQuo();
	private static final AddUSyn   ADD_U_SYN   = new AddUSyn();
	private static final AddLtWrap ADD_LT_WRAP = new AddLtWrap();
	private static final EllipsisC ELLIPSIS_C    = new EllipsisC();
	///*package*/ static final Subr REMOVE_WRAP = new RemoveWrap();

	//
	private static boolean isEllipsis(Datum d) {
		if(d instanceof SymbolName) {
			Symbol smb = ((SymbolName)d).getSymbol();

			return ELLIPSIS.equals(smb);
		} else {
			return false;
		}
	}

	//
	/*private static boolean isUnsyntax(Datum d) {
		if(d instanceof SymbolName) {
			Symbol smb = ((SymbolName)d).getSymbol();

			return (SynQuasisyntax.UNSYNTAX_SPLICING.equals(smb) ||
					SynQuasisyntax.UNSYNTAX.equals(smb));
		} else {
			return false;
		}
	}*/

	//
	private static Datum chkRest(
			Datum c2x,
			PatternDepthMap mp,
			int typ,
			Set<Symbol> st,
			Set<Symbol> reserve) throws PatternEllipsisException {
		Datum c2z = c2x;
		Cons  res = null;
		Cons  rp  = res;

		while(c2z instanceof Cons) {
			Datum c2a = ((Cons)c2z).getCar();
			Cons rx = new Cons();

			if(rp  == null) {
				res = rx;
			} else {
				rp.setCdr(rx);
			}
			rp = rx;

			if(ELLIPSIS.equals(c2a)) {
				throw new LispException("bad ellipsis");
			}
			rp.setCar(compilePattern1(c2a, mp, typ, st, reserve));

			c2z = ((Cons)c2z).getCdr();
		}

		// パラメータで使用されているシンボルを
		// 集合に追加
		if(c2z instanceof Symbol) {
			st.add((Symbol)c2z);
		}
		rp.setCdr(c2z);

		return res;
	}

	//
	private static Datum chkRestVec(
			List<Datum> v2x,
			PatternDepthMap mp,
			int typ,
			Set<Symbol> st,
			Set<Symbol> reserve) throws PatternEllipsisException {
		List<Datum> res = new ArrayList<Datum>();

		for(Datum v2a : v2x) {
			if(isEllipsis(v2a)) {
				throw new LispException("bad ellipsis");
			}
			res.add(compilePattern1(v2a, mp, typ, st, reserve));
		}
		return LispUtils.listToCons(res);
	}

	//
	private static Datum compilePattern1(
			Datum src,
			PatternDepthMap mp,
			int typ,
			Set<Symbol> st,
			Set<Symbol> reserve) throws PatternEllipsisException {
		Datum d = src;
		Datum res = null;
		Cons  rp  = null;

		while(true) {
			if(d instanceof Cons) {
				Cons c = (Cons)d;
				Cons n = new Cons();
				Datum car;

				if(isEllipsis(c.getCar())) {
					throw new PatternEllipsisException();
				}

				// car部をコンパイル
				car = compilePattern1(c.getCar(), mp, typ, st, reserve);

				// cadr部が"..."のときは繰り返しとする
				if(c.getCdr() instanceof Cons) {
					Cons c2 = (Cons)c.getCdr();

					if(isEllipsis(c2.getCar())) {
						Datum n2;

						if(c.getCar() instanceof Symbol) {
							if(reserve.contains((Symbol)c.getCar())) {
								throw new PatternEllipsisException();
							}
						}

						if(typ == PATTERN) {
							// cddrがNILまたはAtomでないときはエラー
							// 将来は削除予定
							if(c2.getCdr() instanceof Cons) {
								//throw new LispException("bad ellipsis");
								Datum xa = chkRest(
										c2.getCdr(), mp, typ, st,
										reserve);

								n2 = new PatternIntEllipsis(car, xa);
							} else {
								// パラメータで使用されているシンボルを
								// 集合に追加
								if(c2.getCdr() instanceof Symbol) {
									st.add((Symbol)c2.getCdr());
								}

								// 省略記号追加
								n2 = new PatternIntEllipsis(
										car, c2.getCdr());
							}
						} else if(typ == TEMPLATE) {
							Datum cdr2;

							// 繰り返しシンボルがないときは
							// 0回以上の繰り返しの可能性
							//if(!car2.useprm) {
								//throw new LispException(
								//		"no repetition symbol");
							//}

							cdr2 = compilePattern1(
									c2.getCdr(), mp, typ, st, reserve);
							n2 = new PatternIntEllipsis(car, cdr2);
						} else {
							throw new RuntimeException();
						}

						// cdr部に追加して脱出
						if(rp == null) {
							res = n2;
							//res.useprm = res.useprm || car2.useprm;
						} else {
							rp.setCdr(n2);
						}
						break;
					}
				}
				n.setCar(car);

				// cdrに追加
				if(rp == null) {
					res = rp = n;
					//res.useprm = res.useprm || car2.useprm;
				} else {
					rp.setCdr(n);
					rp = n;
				}
				d = c.getCdr();
			} else if(d instanceof LispVector) {
				LispVector v = (LispVector)d;
				List<Datum> rv = new ArrayList<Datum>();

				for(int i = 0; i < v.size(); i++) {
					Datum v1;

					v1 = compilePattern1(v.get(i), mp, typ, st, reserve);

					if(i < v.size() - 1 && isEllipsis(v.get(i + 1))) {
						if(v.get(i) instanceof Symbol) {
							if(reserve.contains((Symbol)v.get(i))) {
								throw new PatternEllipsisException(
										((Symbol)v.get(i)).getName());
							}
						}

						if(typ == PATTERN) {
							/*// 終端でないときはエラー
							// 将来は削除予定
							if(i + 1 < v.size() - 1) {
								//throw new LispException("bad ellipsis");
							} else {
								// パラメータで使用されているシンボルを
								// 集合に追加
								//if(v.get(i + 1) instanceof Symbol) {
								//	st.add((Symbol)v.get(i + 1));
								//}

								// 省略記号追加
								rv.add(new PatternIntEllipsis(
										v1,
										Collections.<Datum>emptyList()));
							}*/
							List<Datum> vv =
								v.getList().subList(i + 2, v.size());
							Datum vr =
								chkRestVec(vv, mp, typ, st, reserve);

							rv.add(new PatternIntEllipsis(v1, vr, true));
						} else if(typ == TEMPLATE) {
							rv.add(new PatternIntEllipsis(
									v1, Nil.NIL, true));
						} else {
							throw new RuntimeException();
						}
						i++;
					} else {
						rv.add(v1);
					}
				}
				res = new LispVector(rv);
				break;
			} else {
				// パラメータで使用されているシンボルを集合に追加
				if(d instanceof Symbol) {
					//if(mp.contains((Symbol)d)) {
					//	res.useprm = true;
					//}
					st.add((Symbol)d);
				} else if(d instanceof SymbolScope) {
					SymbolScope ssp = (SymbolScope)d;

					st.add(ssp.getSymbol());
				}

				if(rp == null) {
					res = d;
				} else {
					rp.setCdr(d);
				}
				break;
			}
		}

		return res;
	}

	/**
	 * 
	 * @param d
	 * @param mp
	 * @param st
	 * @param reserve
	 * @return
	 * @throws PatternEllipsisException
	 */
	public static Datum compilePattern(
			Datum d,
			PatternDepthMap mp,
			Set<Symbol> st,
			Set<Symbol> reserve) throws PatternEllipsisException {
		return compilePattern1(d, mp, PATTERN, st, reserve);
	}

	/**
	 * 
	 * @param d
	 * @param mp
	 * @return
	 */
	public static Datum compileTemplate(Datum d, PatternDepthMap mp) {
		Set<Symbol> sz = NullSet.getInstance();
		Set<Symbol> em = Collections.emptySet();

		try {
			return compilePattern1(d, mp, TEMPLATE, sz, em);
		} catch (PatternEllipsisException e) {
			throw new RuntimeException("Internal error");
		}
	}

	//
	private static void searchLevel0(
			Datum in,
			int lev,
			Map<Symbol, Integer> res,
			Set<Symbol> st) {
		Datum sp = in;

		while(true) {
			while(true) {
				if(sp instanceof Cons) {
					Cons spc = (Cons)sp;

					searchLevel0(spc.getCar(), lev, res, st);
					sp = spc.getCdr();
				} else if(sp instanceof LispVector) {
					LispVector v = (LispVector)sp;

					for(int i = 0; i < v.size(); i++) {
						searchLevel0(v.get(i), lev, res, st);
					}
					return;
				} else if(sp instanceof PatternIntEllipsis) {
					// "..."のマッチング
					PatternIntEllipsis si = (PatternIntEllipsis)sp;

					searchLevel0(
							si.getEllipsisList(), lev + 1, res, st);
					sp = si.getCdr();
				} else if(sp instanceof Symbol && st.contains(sp)) {
					Symbol sy = (Symbol)sp;

					res.put(sy, lev);
					return;
				} else {
					return;
				}
			}
		}
	}

	/**
	 * 
	 * @param pat
	 * @param tpl
	 * @param st
	 * @throws PatternDepthException
	 */
	public static void validateLevel(
			Datum pat,
			Datum tpl,
			Set<Symbol> st) throws PatternDepthException {
		Map<Symbol, Integer> p0 = new HashMap<Symbol, Integer>();
		Map<Symbol, Integer> t0 = new HashMap<Symbol, Integer>();

		searchLevel0(pat, 0, p0, st);
		searchLevel0(tpl, 0, t0, st);
		for(Map.Entry<Symbol, Integer> d : p0.entrySet()) {
			if(t0.containsKey(d.getKey())) {
				int ddepth = t0.get(d.getKey());

				if(ddepth != d.getValue()) {
					throw new PatternDepthException(
							d.getKey().getName());
				}
			}
		}
	}

	//
	private static void collectParam1(
			Datum src, Set<Symbol> col, Set<Symbol> reserved) {
		Datum sp = src;

		while(true) {
			if(sp instanceof Cons) {
				Cons spc = (Cons)sp;

				collectParam1(spc.getCar(), col, reserved);
				sp = spc.getCdr();
			} else if(sp instanceof LispVector) {
				LispVector v = (LispVector)sp;

				for(int i = 0; i < v.size(); i++) {
					collectParam1(v.get(i), col, reserved);
				}
				return;
			} else if(sp instanceof PatternIntEllipsis) {
				// "..."のマッチング
				PatternIntEllipsis si = (PatternIntEllipsis)sp;

				collectParam1(si.getEllipsisList(), col, reserved);
				sp = si.getCdr();
			} else if(sp instanceof SymbolName) {
				Symbol smb = ((SymbolName)sp).getSymbol();

				if(!reserved.contains(smb)) {
					col.add((Symbol)smb);
				}
				return;
			} else {
				return;
			}
		}
	}

	//
	private static Set<Symbol> collectParam1(
			Datum src, Set<Symbol> reserved) {
		Set<Symbol> col = new HashSet<Symbol>();

		collectParam1(src, col, reserved);
		return col;
	}

	//
	private static boolean match(
			Datum src,
			Datum dest,
			PatternDepthMap mp,
			PatternDepthIndex index,  // 「深さ数」インデックス
			Set<Symbol> reserved) {
		Datum sp = src;
		Datum dp = dest;

		while(true) {
			if(sp instanceof Cons) {
				Cons spc = (Cons)sp;

				if(dp instanceof Cons) {
					// 構造が同じ可能性
					Cons dpc = (Cons)dp;
					boolean bl;

					bl = match(
							spc.getCar(), dpc.getCar(),
							mp, index, reserved);
					if(bl) {
						sp = spc.getCdr();
						dp = dpc.getCdr();
					} else {
						// car部の構造が違った
						return false;
					}
				} else {
					// 構造が違う
					return false;
				}
			} else if(sp instanceof LispVector) {
				LispVector vs = (LispVector)sp;
				LispVector vd;

				if(!(dp instanceof LispVector)) {
					// 構造が違う
					return false;
				}

				vd = (LispVector)dp;
				int i = 0;
				for(; i < vs.size(); i++) {
					Datum dd = vs.get(i);

					if(dd instanceof PatternIntEllipsis) {
						PatternIntEllipsis si = (PatternIntEllipsis)dd;

						PatternDepthIndex nind = index.addDepthNew();
						List<Datum> vv =
							LispUtils.consToListIgnoreDot(si.getCdr());
						int j = i;

						for(; j < (vd.size() - vv.size()); j++) {
							boolean mtr;

							mtr = match(
									si.getEllipsisList(),
									vd.get(j),
									mp, nind, reserved);
							if(mtr) {
								nind = nind.incNew();
							} else {
								return false;
							}
						}

						// マッチした
						// [シンボル, 直前の深さの深さ数]→要素数を記録
						Set<Symbol> col = collectParam1(
								si.getEllipsisList(), reserved);
						int rep = nind.pop();

						mp.setRepetaion(col, nind, rep);

						// match the rest of vector
						for(int k = 0; j < vd.size(); j++, k++) {
							boolean mtr;

							mtr = match(
									vv.get(k),
									vd.get(j),
									mp, index, reserved);
							if(!mtr) {
								return false;
							}
						}
						return true;
					} else if(i >= vd.size()) {
						return false;
					} else {
						boolean bl;

						bl = match(
								vs.get(i), vd.get(i),
								mp, index, reserved);
						if(!bl) {
							// 構造が違った
							return false;
						}
					}
				}
				return i == vd.size();
			} else if(sp instanceof PatternIntEllipsis) {
				// "..."のマッチング
				PatternIntEllipsis si = (PatternIntEllipsis)sp;
				Datum pt = dp;
				PatternDepthIndex nind = index.addDepthNew();

				// match repeat 0 times
				if(match(si.getCdr(), pt, mp, index, reserved)) {
					Set<Symbol> col = collectParam1(
							si.getEllipsisList(), reserved);
					int rep = nind.pop();

					// マッチした
					// [シンボル, 直前の深さの深さ数]→要素数を記録
					mp.setRepetaion(col, nind, rep);
					return true;
				}

				while(true) {
					if(pt instanceof Cons) {
						// 繰り返しにマッチ
						Cons ptc = (Cons)pt;
						boolean mtr;

						mtr = match(
								si.getEllipsisList(), ptc.getCar(),
								mp, nind, reserved);
						if(mtr) {
							pt = ptc.getCdr();
							nind = nind.incNew();
						} else {
							return false;
						}

						// cdr部をマッチ
						boolean rx = match(
								si.getCdr(), ptc.getCdr(),
								mp, index, reserved);

						if(rx) {
							Set<Symbol> col = collectParam1(
									si.getEllipsisList(), reserved);
							int rep = nind.pop();

							// マッチした
							// [シンボル, 直前の深さの深さ数]→要素数を記録
							mp.setRepetaion(col, nind, rep);
							return true;
						}
					} else {
						// cdr部をマッチ
						Set<Symbol> col = collectParam1(
								si.getEllipsisList(), reserved);
						int rep = nind.pop();

						// マッチした
						// [シンボル, 直前の深さの深さ数]→要素数を記録
						mp.setRepetaion(col, nind, rep);
						return match(
								si.getCdr(), pt,
								mp, index, reserved);
					}
				}
			} else if(sp instanceof Symbol && !reserved.contains(sp)) {
				// dpにマークを付ける
				mp.put((Symbol)sp, index, dp);
				return true;
			} else if(sp instanceof SymbolScope) {
				SymbolScope ssp = (SymbolScope)sp;

				// dpにマークを付ける
				mp.put(ssp.getSymbol(), index, dp);
				return true;
			} else if(sp.equals(UNDERBAR)) {
				return true;
			} else {
				// return (equal? sp dp)
				return sp.isEqv(dp);
			}
		}
	}

	/**
	 * 
	 * @param src
	 * @param dest
	 * @param mp
	 * @param reserved
	 * @return
	 */
	public static boolean match(
			Datum src,
			Datum dest,
			PatternDepthMap mp,
			Set<Symbol> reserved) {
		return match(src, dest, mp, new PatternDepthIndex(), reserved);
	}

	//
	private static void collectParam2(
			Datum src, Set<Symbol> col, PatternDepthMap mp) {
		Datum sp = src;

		while(true) {
			if(sp instanceof Cons) {
				Cons spc = (Cons)sp;

				collectParam2(spc.getCar(), col, mp);
				sp = spc.getCdr();
			} else if(sp instanceof LispVector) {
				LispVector v = (LispVector)sp;

				for(int i = 0; i < v.size(); i++) {
					collectParam2(v.get(i), col, mp);
				}
				return;
			} else if(sp instanceof PatternIntEllipsis) {
				// "..."のマッチング
				PatternIntEllipsis si = (PatternIntEllipsis)sp;

				collectParam2(si.getEllipsisList(), col, mp);
				sp = si.getCdr();
			} else if(sp instanceof Symbol) {
				if(mp.contains((Symbol)sp)) {
					col.add((Symbol)sp);
				}
				return;
			} else if(sp instanceof ExWrap) {
				Datum dd = ((ExWrap)sp).getWrapee();

				if(dd instanceof SymbolName) {
					Symbol sy = (Symbol)((SymbolName)dd).getSymbol();

					if(mp.contains(sy)) {
						col.add(sy);
					}
				}
				return;
			} else {
				return;
			}
		}
	}

	//
	private static Set<Symbol> collectParam2(
			Datum src, PatternDepthMap mp) {
		Set<Symbol> col = new HashSet<Symbol>();

		collectParam2(src, col, mp);
		return col;
	}

	//--------------------------------------------
	// expand
	//--------------------------------------------
	private static class ExWrap extends Datum {

		private Datum wrapee;

		private ExWrap(Datum w) {
			wrapee = w;
		}

		public Datum getWrapee() {
			return wrapee;
		}

		public String toString() {
			return "w(" + wrapee + ")";
		}
	}

	//
	/*package*/ static class IndSym extends Datum {

		private Symbol wrapee;
		private PatternDepthIndex index;

		private IndSym(Symbol w, PatternDepthIndex ind) {
			this.wrapee = w;
			this.index  = ind;
		}

		//
		public Symbol getWrapee() {
			return wrapee;
		}

		//
		public PatternDepthIndex getIndex() {
			return index;
		}

		public String toString() {
			return wrapee + "(" + index + ")";
		}

	}

	//
	private static boolean checkEllipsis(
			Datum d,
			PatternDepthMap args) {
		if(d instanceof Cons) {
			ConsIterator itr = new ConsIterator(d);
			boolean res = false;

			while(itr.hasNext()) {
				Datum d0 = itr.next();

				res = res || checkEllipsis(d0, args);
			}
			return res || checkEllipsis(itr.getTerminal(), args);
		} else if(d instanceof PatternIntEllipsis) {
			PatternIntEllipsis e0 = (PatternIntEllipsis)d;
			boolean res;

			res = checkEllipsis(e0.getEllipsisList(), args);
			return res && checkEllipsis(e0.getCdr(), args);
		} else if(d instanceof SymbolName) {
			return args.contains(((SymbolName)d).getSymbol());
		} else {
			return true;
		}
	}

	// expand1を実行した後は必ずnullチェックする
	private static Datum expand1(
			Datum templ,
			PatternDepthMap args,
			PatternDepthIndex index,
			UserSyntax usyn,
			Map<Symbol, Symbol> box,
			Environment env,
			int level,
			Symbol.Namespace nsp,
			boolean syncase) throws PatternDepthException {
		Datum sp = templ;
		ConsListBuilder bld1 = new ConsListBuilder();

		while(true) {
			//System.out.println("aaa:" + templ);
			//System.out.println("arg:" + args);
			if(sp instanceof Cons) {
				// コンスリスト
				Cons spc = (Cons)sp;
				Cons n = new Cons();
				Datum car;

				// syntax-case
//				if(spc.getCar().equals(SynSyntaxCase.SYNTAX_CASE)) {
////				return decodeEllipsis(spc, args);
//					return sp;
//				} else
				if(spc.getCar() instanceof SymbolName) {
					Symbol sy2 = ((SymbolName)spc.getCar()).getSymbol();
					Datum  dd2 = env.findDatum(sy2);
					//int lll = level;

					if(dd2 instanceof SynLetType) {
						sp = spc.getCdr();
					} else if(dd2 instanceof SynLambda) {
						sp = spc.getCdr();
					} else if(dd2 instanceof SynSyntaxCase) {
						if(level != 0) {
							sp = spc.getCdr();
						} else {
							sp = spc.getCdr();
						}
					} else if(dd2 instanceof SynSyntax) {
						level = -1;
						sp = spc.getCdr();
					} else if(dd2 instanceof SynQuasisyntax) {
						level = (level == -1) ? -1 : level + 1;
						sp = spc.getCdr();
					} else if(SynQuasisyntax.UNSYNTAX.equals(sy2)) {
						level = (level > 0) ? level - 1 : level;
						sp = spc.getCdr();
					} else if(SynQuasisyntax.UNSYNTAX_SPLICING.equals(
							sy2)) {
						level = (level > 0) ? level - 1 : level;
						sp = spc.getCdr();
					} else {
						sp = spc.getCdr();
					}

					car = expand1(
							spc.getCar(), args, index, usyn, box, env,
							level, nsp, syncase);
					bld1.append(car);
				} else {
					car = expand1(
							spc.getCar(), args, index, usyn, box, env,
							level, nsp, syncase);
					if(car == null) {
						return null;
					} else {
						n.setCar(car);
						bld1.appendCons(n);
					}

					sp = spc.getCdr();
				}
			} else if(sp instanceof LispVector) {
				LispVector  ve = (LispVector)sp;
				List<Datum> vr = new ArrayList<Datum>();

				for(int j = 0; j < ve.size(); j++) {
					Datum dd = ve.get(j);

					if(dd instanceof PatternIntEllipsis) {
						// "..."の展開
						PatternIntEllipsis si = (PatternIntEllipsis)dd;

						// 含まれない
						if(!checkEllipsis(si.getEllipsisList(),
								args)) {
							Datum car = expand1(
									si.getEllipsisList(),
									args, index, usyn, box, env,
									level, nsp, syncase);
							vr.add(car);
							vr.add(ELLIPSIS);
						}

						//
						Set<Symbol> params =
							collectParam2(si.getEllipsisList(), args);

						// [シンボル, 直前の深さの深さ数]
						//   →要素数の最小値を取得
						int rep = args.getRepetaion(params, index);

						// 繰り返しの展開
						index.addDepth();  // 深さを追加(掘る)
						for(int i = 0; i < rep; i++, index.inc()) {
							Datum rpt = expand1(
									si.getEllipsisList(),
									args, index, usyn, box, env,
									level, nsp, syncase);

							if(rpt == null) {
								int v = index.pop();

								if(v > 0) {
									break;
								} else {
									// 1回も展開されなかった
									return null;
								}
							} else {
								vr.add(rpt);
							}
						}
						index.pop();
					} else {
						Datum d3;

						d3 = expand1(
								ve.get(j), args, index, usyn, box, env,
								level, nsp, syncase);
						if(d3 == null) {
							return null;
						} else {
							vr.add(d3);
						}
					}
				}
				return new LispVector(vr);
			} else if(sp instanceof PatternIntEllipsis) {
				// "..."の展開
				PatternIntEllipsis si = (PatternIntEllipsis)sp;
				ConsListBuilder bld2 = new ConsListBuilder();

				// 含まれない
				if(!checkEllipsis(si.getEllipsisList(), args)) {
					Datum ddz = gtWrap(si.getEllipsisList());

					//System.out.println("dssa:" + si.getEllipsisList());
					Datum car = expand1(
							ddz,
							args, index, usyn, box, env,
							level, nsp, syncase);
					bld2.append(car);
					bld2.append(ELLIPSIS);

					// cdr部の展開
					Datum cdr = expand1(
							si.getCdr(),
							args, index, usyn, box, env,
							level, nsp, syncase);
					return bld1.get(bld2.get(cdr));
				}

				//
				Set<Symbol> params = collectParam2(
						si.getEllipsisList(), args);

				// [シンボル, 直前の深さの深さ数]→要素数の最小値を取得
				int rep = args.getRepetaion(params, index);

				// 繰り返しの展開
				index.addDepth();  // 深さを追加(掘る)
				for(int i = 0; i < rep; i++, index.inc()) {
					Cons n = new Cons();
					Datum rpt = expand1(
							si.getEllipsisList(),
							args, index, usyn, box, env,
							level, nsp, syncase);

					if(rpt == null) {
						int v = index.pop();

						if(v > 0) {
							break;
						//} else if(index.isAllZero()) {
						//	return Undef.UNDEF2;
						} else {
							// 1回も展開されなかった
							return null;
						}
					} else {
						n.setCar(rpt);
						bld2.appendCons(n);
					}
				}
				index.pop();

				// cdr部の展開
				Datum cdr = expand1(
						si.getCdr(), args, index, usyn, box, env,
						level, nsp, syncase);

				if(cdr == null) {
					return null;
				} else {
					return bld1.get(bld2.get(cdr));
				}
			//} else if(Symbol.getSymbol("a:it").equals(sp)) {  // test
			} else if(sp instanceof SymbolScope) {
				SymbolScope ssp = (SymbolScope)sp;
				Symbol      ss  = ssp.getSymbol();

				if(args.contains(ss)) {
					Datum dt = args.get(ss, index);

					// マークを付ける
					dt = PatternMatch.markReplace(box, dt, true);
					//dt = new Wrap(dt);
					return bld1.get(dt);
				} else {
					//dt = bld1.get(new SymbolScope(
					//		(Symbol)sp, usyn, true));
					//box.put(ss, ss);
					//return bld1.get(ss);
					return bld1.get(sp);
				}
			} else if(sp instanceof Symbol) {
				Symbol sy1 = (Symbol)sp;

				if(usyn.getReservedSet().contains(sy1)) {
					return sy1;
				} else if(args.contains(sy1)) {
					Datum dt = args.get(sy1, index);

					// マークを付ける
					dt = PatternMatch.markReplace(box, dt, true);
					return bld1.get(dt);
				} else if(sy1.isGenerated()) {
					return bld1.get(sy1);
				} else {
					//menv.bindDatum(sy1, Symbol.gensym());
					//return bld1.get(new SymbolScope(sy1, usyn));
					//return bld1.get(sy1);
					if(index.depth() == 0) {
//						Symbol s2 = sy1;
						Symbol s2 = nsp.getSymbol(sy1.getName());

						return bld1.get(syncase ? sy1 : s2);
					} else {
						//IndSym isym = new IndSym(sy1, index.copy());
						IndSym isym = new IndSym(
								nsp.getSymbol(sy1.getName()),
								index.copy());

						return bld1.get(isym);
					}
				}
			} else if(sp instanceof ExWrap) {
				Datum wpe = ((ExWrap)sp).getWrapee();

				if(wpe instanceof SymbolName) {
					Symbol smb = ((SymbolName)wpe).getSymbol();

					if(args.contains(smb)) {
						Datum dt = args.get(smb, index);

						// マークを付ける
						dt = PatternMatch.markReplace(box, dt, true);
						return bld1.get(dt);
					} else {
						if(index.depth() == 0) {
//							Symbol s2 = smb;
							Symbol s2 = nsp.getSymbol(smb.getName());

							return bld1.get(syncase ? smb : s2);
						} else {
							//IndSym isym =
							//	new IndSym(smb, index.copy());
							IndSym isym = new IndSym(
									nsp.getSymbol(smb.getName()),
									index.copy());

							return bld1.get(isym);
						}
					}
				}
				return bld1.get(wpe);
			} else if(sp instanceof IndSym) {
				IndSym ism = (IndSym)sp;
				Datum dt = args.get(
						(Symbol)ism.getWrapee(), ism.getIndex());

				// マークを付ける
				dt = PatternMatch.markReplace(box, dt, true);
				return bld1.get(dt);
			} else if(sp instanceof LtWrap) {
				Datum wpe = ((LtWrap)sp).getWrapee();

				return bld1.get(wpe);
			} else {
				return bld1.get(sp);
			}
		}
	}

	/**
	 * 
	 * @param templ
	 * @param args
	 * @param usyn
	 * @param box
	 * @param env
	 * @return
	 * @throws PatternDepthException
	 */
	public static Datum expand(
			Datum templ,
			PatternDepthMap args,
			UserSyntax usyn,
			Map<Symbol, Symbol> box,
			Environment env,
			boolean syncase) throws PatternDepthException {
		Datum res = expand1(
				templ, args, new PatternDepthIndex(),
				usyn, box, env, 0, new Symbol.Namespace(), syncase);

		if(res == null) {
			throw new LispException("syntax expansion has failed");
		}
		return res;
	}

	/**
	 * スコープ情報を追加する。
	 * 
	 * @param src
	 * @return
	 */
	public static Datum appendScope(
			Datum src, PatternDepthMap args, UserSyntax usyn) {
		Datum sp = src;
		ConsListBuilder bld1 = new ConsListBuilder();

		while(true) {
			if(sp instanceof Cons) {
				Cons spc = (Cons)sp;
				Cons app = new Cons();

				app.setCar(appendScope(spc.getCar(), args, usyn));
				bld1.appendCons(app);
				sp = spc.getCdr();
			} else if(sp instanceof LispVector) {
				LispVector v = (LispVector)sp;
				List<Datum> vr = new ArrayList<Datum>();

				for(int i = 0; i < v.size(); i++) {
					vr.add(appendScope(v.get(i), args, usyn));
				}
				return new LispVector(vr);
			} else if(sp instanceof Symbol) {
				Symbol sy1 = (Symbol)sp;

				if(sy1.isReplaced()) {
					return bld1.get(sy1);
				} else if(sy1.isGenerated()) {
					return bld1.get(sy1);
				} else {
					return bld1.get(new SymbolScope(sy1, usyn));
					//return bld1.get(sy1);
				}
			} else if(sp instanceof SyntaxUtils.SafeWrap) {
				return ((SyntaxUtils.SafeWrap)sp).getWrapee();
			} else {
				return bld1.get(sp);
			}
		}
	}

	//
	private static final Scheme AUX_S;

	static {
		AUX_S = new Scheme(
				Scheme.newRnRSEnv(Scheme.SCHEME_VERSION),
				LispMessage.getInstance());

		AUX_S.set("syncase?", IS_SYN_CASE);
		AUX_S.set("synquot?", IS_SYN_QUOT);
		AUX_S.set("withsyn?", IS_WITH_SYN);
		AUX_S.set("synqquo?", IS_SYN_QQUT);
		AUX_S.set("addusyn",  ADD_U_SYN);
		AUX_S.set("ltwrap",   ADD_LT_WRAP);
		AUX_S.set("get...",   ELLIPSIS_C);
		AUX_S.set("gensym",   new Gensym());
		AUX_S.exec(
				"(define (mapv f vec)" +
				"  (let ((v1 (make-vector (vector-length vec))))" +
				"    (let loop ((i (vector-length vec)))" +
				"      (cond ((zero? i) v1)" +
				"            (else" +
				"              (vector-set!" +
				"                v1" +
				"                (- i 1)" +
				"                (f (vector-ref vec (- i 1))))" +
				"              (loop (- i 1)))))))");
		AUX_S.exec(
				"(define (flat*-aux x y)" +
				"  (cond ((null? x) y)" +
//				"        ((and (pair? x) (pair? (car x)))" +
//				"          (flat*-aux (car x) (flat*-aux (cdr x) y)))" +
				"        ((pair? x)" +
				"          (append" +
				"            (flat*-aux (car x) '())" +
				"            (flat*-aux (cdr x) y)))" +
				"        ((vector? x)" +
				"          (let lp2 ((i (vector-length x)) (rs y))" +
				"            (cond ((zero? i) rs)" +
				"                  (else" +
				"                    (lp2 (- i 1)" +
				"                         (append" +
				"                           (flat*-aux" +
				"                             (vector-ref x (- i 1))" +
				"                             '())" +
				"                           rs))))))" +
				"        (else (cons x y))))");
		AUX_S.exec(
				"(define (flat* x)" +
				"  (flat*-aux x '()))");
		AUX_S.exec(
				"(define (remnsym x)" +
				"  (cond ((null? x) '())" +
				"        ((and (pair? x) (symbol? (car x)))" +
				"          (cons (car x) (remnsym (cdr x))))" +
				"        ((pair? x) (remnsym (cdr x)))" +
				"        (else x)))");
		AUX_S.exec(
				"(define (cpy x)" +
				"  (cond ((null? x) '())" +
				"        (else (cons x (cpy (cdr x))))))");
		AUX_S.exec(
				"(define (properize x)" +
				"  (cond ((null? x) '())" +
				"        ((pair? x)" +
				"          (cons (car x) (properize (cdr x))))" +
				"        (else (cons x '()))))");
		AUX_S.exec(
				"(define (coll x) (properize (car x)))");
		AUX_S.exec(
				"(define (unsyn? x)" +
				"  (or (eq? x 'unsyntax) (eq? x 'unsyntax-splicing)))");
		AUX_S.exec(
				"(define (repls x usyn)" +
				//"  (let ((prms '()))" +
				"  (let loop ((x x) (q #f) (prms '()))" +
				"    (letrec ((f1" +
				"               (lambda (x prms)" +
				//"(display (flat* (map coll (cdddr x))))" +
				"                 (append" +
				"                   (remnsym (flat* (caddr x)))" +
				"                   (remnsym (flat*" +
				"                     (map coll (cdddr x))))" +
				"                   prms)))" +
				"             (f2" +
				"               (lambda (x prms)" +
				"                 (append" +
				"                   (remnsym (flat*" +
				"                     (map coll (cadr x))))" +
				"                   prms))))" +
				//"        (display prms) (newline)" +
				"      (cond ((null? x) '())" +
				"            ((eq? x '_) x)" +
				"            ((eq? x (get...)) (get...))" +
				"            ((and (pair? x) (syncase? (car x)))" +
				"              (cons (car x)" +
				"                    (loop (cdr x) q (f1 x prms))))" +
				"            ((and (pair? x) (withsyn? (car x)))" +
				"              (cons (car x)" +
				"                    (loop (cdr x) q (f2 x prms))))" +
				"            ((and (pair? x) (synquot? (car x)))" +
				"              (cons (car x)" +
				"                    (loop (cdr x) #t prms)))" +
				"            ((and (pair? x)" +
				"                  (or (not q) (integer? q))" +
				"                  (synqquo? (car x)))" +
				"              (cons (car x)" +
				"                    (loop (cdr x)" +
				"                          (if q (+ q 1) 0)" +
				"                          prms)))" +
				"            ((and (pair? x) (unsyn? (car x)))" +
				"              (cond ((zero? q) x)" +
				"                    ((integer? q)" +
				"                       (cons" +
				"                         (car x)" +
				"                         (loop (cdr x)" +
				"                               (- q 1)" +
				"                               prms)))" +
				"                    (else (error" +
				"                            (get-default-message" +
				"                              'err.unsyntax.malform" +
				"                              '())))))" +
				"            ((and (pair? x))" +
				"              (cons (loop (car x) q prms)" +
				"                    (loop (cdr x) q prms)))" +
				"            ((vector? x)" +
				"              (mapv (lambda (x) (loop x q prms)) x))" +
				"            ((and q (symbol? x) (not (memq x prms)))" +
				"              (addusyn x usyn))" +
				//"            ((and q (symbol? x) (memq x prms))" +
				//"               (wrap x))" +
				"            (else x)))))");
		AUX_S.exec("(define (letren x) (lren0 x '() #f))");
		AUX_S.exec(
				"(define (lren0 x as lv)" +
		//		"(display as) (newline)" +
				"  (cond ((and (pair? x) (eq? 'syntax (car x)))" +
				"          (cons (car x)" +
				"                (lren0 (cdr x) as #t)))" +
				"        ((and (pair? x) (eq? 'quasisyntax (car x)))" +
				"          (cons (car x)" +
				"                (lren0 (cdr x) as" +
				"                       (if lv lv 0))))" +
				"        ((and (pair? x) (eq? 'unsyntax (car x)))" +
				"          (cons (car x)" +
				"                (lren0 (cdr x) as" +
				"                       (if (> lv 0) (- lv 1) #f))))" +
				"        ((and (pair? x)" +
				"              (eq? 'unsyntax-splicing (car x)))" +
				"          (cons (car x)" +
				"                (lren0 (cdr x) as" +
				"                       (if (> lv 0) (- lv 1) #f))))" +
				"        ((and (not lv)" +
				"              (pair? x)" +
				"              (memq (car x) '(let letrec)))" +
				"          (if (symbol? (cadr x))" +
				"              (chletn x" +
				"                      as" +
				"                      (cons" +
				"                        (cons (cadr x) (gensym))" +
				"                        (append" +
				"                          (makeasc (caddr x))" +
				"                          as))" +
				"                      lv)" +
				"              (chlet  x" +
				"                      as" +
				"                      (cons (makeasc (cadr x)) as)" +
				"                      lv)))" +
				"        ((and (not lv)" +
				"              (pair? x)" +
				"              (memq (car x) '(let* letrec*)))" +
				"          (chlet* x" +
				"                  as" +
//				"                  (cons(makeasc (cadr x)) as)" +
				"                  lv))" +
				"        ((and (not lv)" +
				"              (pair? x)" +
				"              (memq (car x) '(lambda)))" +
				"          (let ((as2 (append (makeascl (cadr x)) as)))" +
				"            (cons (car x)" +
				"             (cons (lren0 (cadr x) as2 lv)" +
				"              (lren0 (cddr x) as2 lv)))))" +
				"        ((pair? x)" +
				"          (cons (lren0 (car x) as lv)" +
				"                (lren0 (cdr x) as lv)))" +
				"        ((vector? x)" +
				"          (mapv (lambda (x) (lren0 x as lv)) x))" +
				"        ((and (not lv) (assq x as))" +
				"          (cdr (assq x as)))" +
				"        (else x)))" +
				"");
		AUX_S.exec(
				"(define (ext-var x)" +
				"  (remnsym (map car x)))");
		AUX_S.exec(
				"(define (makeasc x)" +
				"  (cond ((null? x) '())" +
				"        (else (cons (cons (caar x) (gensym))" +
				"                    (makeasc (cdr x))))))");
		AUX_S.exec(
				"(define (makeascl x)" +
				"  (cond ((null? x) '())" +
				"        (else (cons (cons (car x) (gensym))" +
				"                    (makeascl (cdr x))))))");
		AUX_S.exec(
				"(define (chlet x a0 as lv)" +
				"  (cons (car x)" +
				"        (cons (map (lambda (x)" +
				"                     (cons (assqif (car x) as)" +
				"                           (lren0 (cdr x) a0 lv)))" +
				"                   (cadr x))" +
				"              (lren0 (cddr x) as lv))))");
		AUX_S.exec(
				"(define (chlet*-aux x a0 lv)" +
//				"(display a0) (newline)" +
				"  (cond ((null? x) (values '() a0))" +
				"        (else" +
				"          (let ((a1 (cons" +
				"                      (cons (caar x) (gensym)) a0)))" +
				"            (call-with-values" +
				"              (lambda () (chlet*-aux (cdr x) a1 lv))" +
				"              (lambda (rs a2)" +
				"                (values" +
				"                  (cons" +
				"                    (cons (assqif (caar x) a1)" +
				"                          (lren0 (cdar x) a0 lv))" +
				"                    rs)" +
				"                  a2)))))))");
		AUX_S.exec(
				"(define (chlet* x a0 lv)" +
				"  (cons (car x)" +
				"        (call-with-values" +
				"          (lambda () (chlet*-aux (cadr x) a0 lv))" +
				"          (lambda (rs as)" +
				"            (cons rs" +
				"                  (lren0 (cddr x) as lv))))))");
		AUX_S.exec(
				"(define (chletn x a0 as lv)" +
				"  (cons (car x)" +
				"        (cons (cdr (assq (cadr x) as))" +
				"              (cons (map (lambda (x)" +
				"                           (cons (assqif (car x) as)" +
				"                                 (lren0" +
				"                                   (cdr x) a0 lv)))" +
				"                         (caddr x))" +
				"                    (lren0 (cdddr x) as lv)))))");
		AUX_S.exec(
				"(define (assqif x as)" +
				"  (if (assq x as) (cdr (assq x as)) x))");
	}

	/**
	 * スコープ情報を追加する。
	 * 
	 * @param src
	 * @return
	 */
	public static Datum appendScopeCase(
			Datum src, UserSyntax usyn) {
		Datum re0 = AUX_S.call("letren", src);
//		System.out.println(re0);
		Datum re2 = AUX_S2.call("letren", re0);
//		System.out.println("r2:" + re2);
		Datum res = AUX_S.call("repls",  re2, usyn);
//		System.out.println("rp:" + res);

		return res;
	}

	/**
	 * evalなどのためにスコープ情報を取り除く。
	 * 
	 * @param src
	 * @return
	 */
	public static Datum removeScope(Datum src) {
		Datum sp = src;
		ConsListBuilder bld1 = new ConsListBuilder();

		while(true) {
			if(sp instanceof Cons) {
				Cons spc = (Cons)sp;
				Cons app = new Cons();

				app.setCar(removeScope(spc.getCar()));
				bld1.appendCons(app);
				sp = spc.getCdr();
			} else if(sp instanceof LispVector) {
				LispVector v = (LispVector)sp;
				List<Datum> vr = new ArrayList<Datum>();

				for(int i = 0; i < v.size(); i++) {
					vr.add(removeScope(v.get(i)));
				}
				return new LispVector(vr);
			} else if(sp instanceof SymbolScope) {
				return bld1.get(((SymbolScope)sp).getSymbol());
			} else {
				return bld1.get(sp);
			}
		}
	}

	/**
	 * 
	 * @param box
	 * @param src
	 * @param mark
	 * @return
	 */
	public static Datum markReplace(
			Map<Symbol, Symbol> box, Datum src, boolean mark) {
		Datum sp = src;
		ConsListBuilder bld1 = new ConsListBuilder();

		while(true) {
			if(sp instanceof Cons) {
				Cons spc = (Cons)sp;
				Cons app = new Cons();

				app.setCar(markReplace(box, spc.getCar(), mark));
				bld1.appendCons(app);
				sp = spc.getCdr();
			} else if(sp instanceof LispVector) {
				LispVector v = (LispVector)sp;
				List<Datum> vr = new ArrayList<Datum>();

				for(int i = 0; i < v.size(); i++) {
					vr.add(markReplace(box, v.get(i), mark));
				}
				return new LispVector(vr);
			} else if(sp instanceof Symbol) {
				Symbol mk = Symbol.newAndMark(box, (Symbol)sp, mark);

				return bld1.get(mk);
			} else {
				return bld1.get(sp);
			}
		}
	}

	/**
	 * 
	 * @param src
	 * @return
	 */
	public static Datum gtWrap(Datum src) {
		Datum sp = src;
		ConsListBuilder bld1 = new ConsListBuilder();

		while(true) {
			if(sp instanceof Cons) {
				Cons spc = (Cons)sp;
				Cons app = new Cons();

				app.setCar(gtWrap(spc.getCar()));
				bld1.appendCons(app);
				sp = spc.getCdr();
			} else if(sp instanceof LispVector) {
				LispVector v = (LispVector)sp;
				List<Datum> vr = new ArrayList<Datum>();

				for(int i = 0; i < v.size(); i++) {
					vr.add(gtWrap(v.get(i)));
				}
				return new LispVector(vr);
			} else if(sp instanceof Symbol) {
				return bld1.get(new GtWrap((Symbol)sp));
			} else {
				return bld1.get(sp);
			}
		}
	}

	/**
	 * 
	 * @param src
	 * @return
	 */
	public static Datum gtUnwrap(Datum src) {
		Datum sp = src;
		ConsListBuilder bld1 = new ConsListBuilder();

		while(true) {
			if(sp instanceof Cons) {
				Cons spc = (Cons)sp;
				Cons app = new Cons();

				app.setCar(gtUnwrap(spc.getCar()));
				bld1.appendCons(app);
				sp = spc.getCdr();
			} else if(sp instanceof LispVector) {
				LispVector v = (LispVector)sp;
				List<Datum> vr = new ArrayList<Datum>();

				for(int i = 0; i < v.size(); i++) {
					vr.add(gtUnwrap(v.get(i)));
				}
				return new LispVector(vr);
			} else if(sp instanceof GtWrap) {
				return bld1.get(((GtWrap)sp).getWrapee());
			} else if(sp instanceof IndSym) {
				return ((IndSym)sp).wrapee;
			} else {
				return bld1.get(sp);
			}
		}
	}

	/*public static Datum removeWrap(Datum src) {
		Datum sp = src;
		ConsListBuilder bld1 = new ConsListBuilder();

		while(true) {
			if(sp instanceof Cons) {
				Cons spc = (Cons)sp;
				Cons app = new Cons();

				app.setCar(removeWrap(spc.getCar()));
				bld1.appendCons(app);
				sp = spc.getCdr();
			} else if(sp instanceof LispVector) {
				LispVector v = (LispVector)sp;
				List<Datum> vr = new ArrayList<Datum>();

				for(int i = 0; i < v.size(); i++) {
					vr.add(removeWrap(v.get(i)));
				}
				return new LispVector(vr);
			} else if(sp instanceof ExWrap) {
				return bld1.get(((ExWrap)sp).getWrapee());
			} else {
				return bld1.get(sp);
			}
		}
	}*/

	//
	private static final Scheme AUX_S2;

	//
	private static class EqWithSyn extends BinaryArgs {

		@Override
		protected Datum execute(
				Datum c1a, Datum c2a,
				Environment env, LispMessage mesg) {
			if(c1a instanceof SymbolScope &&
					c2a instanceof SymbolScope) {
				SymbolScope s1 = (SymbolScope)c1a;
				SymbolScope s2 = (SymbolScope)c2a;

				return 	LispBoolean.getInstance(
						s1.getSymbol().equals(s2.getSymbol()) &&
						s1.isSameScope(s2));
			} else if(c1a instanceof Symbol && c2a instanceof Symbol) {
				Symbol s1 = (Symbol)c1a;
				Symbol s2 = (Symbol)c2a;

				return LispBoolean.getInstance(s1.equals(s2));
			}
			return LispBoolean.FALSE;
		}

	}

	//
	private static class RmWithSyn extends UnaryArgs {

		@Override
		protected Datum execute(
				Datum c1a,
				Environment env, LispMessage mesg) {
			if(c1a instanceof SymbolScope) {
				SymbolScope s1 = (SymbolScope)c1a;

				return s1.getSymbol();
			}
			return c1a;
		}

	}

	static {
		AUX_S2 = new Scheme(
				Scheme.newRnRSEnv(Scheme.SCHEME_VERSION),
				LispMessage.getInstance());

		AUX_S2.set("gensym",   new Gensym());
		AUX_S2.set("samescp?", new EqWithSyn());
		AUX_S2.set("remvscp",  new RmWithSyn());
		AUX_S2.exec(
				"(define (mapv f vec)" +
				"  (let ((v1 (make-vector (vector-length vec))))" +
				"    (let loop ((i (vector-length vec)))" +
				"      (cond ((zero? i) v1)" +
				"            (else" +
				"              (vector-set!" +
				"                v1" +
				"                (- i 1)" +
				"                (f (vector-ref vec (- i 1))))" +
				"              (loop (- i 1)))))))");
		AUX_S2.exec(
				"(define (asss x lis)" +
//				"(display (samescp? (caar lis) x)) (newline)" +
				"  (cond ((null? lis) #f)" +
				"        ((not (pair? (car lis)))" +
				"          (error (get-default-message" +
				"            'err.require.pair)))" +
				"        ((samescp? (caar lis) x) (car lis))" +
				"        (else (asss x (cdr lis)))))");
		AUX_S2.exec("(define (letren x) (lren0 x '() #f))");
		AUX_S2.exec(
				"(define (lren0 x as lv)" +
//				"(display x) (newline)" +
//				"(display as) (newline)" +
				"  (cond (lv" +
				"          (cond ((and (pair? x)" +
				"                     (eq? 'unsyntax (remvscp (car x))))" +
				"                  (cons (car x)" +
				"                    (lren0 (cdr x) as" +
				"                           (if (> lv 0) (- lv 1) #f))))" +
				"                ((and (pair? x)" +
				"                     (eq? 'unsyntax-splicing (remvscp (car x))))" +
				"                  (cons (car x)" +
				"                    (lren0 (cdr x) as" +
				"                           (if (> lv 0) (- lv 1) #f))))" +
				"                ((pair? x)" +
				"                  (cons (lren0 (car x) as lv)" +
				"                        (lren0 (cdr x) as lv)))" +
				"                ((vector? x)" +
				"                  (mapv (lambda (x) (lren0 x as lv)) x))" +
				"                ((and lv (asss x as))" +
				"                  (cdr (asss x as)))" +
				"                (else x)))" +
				"        ((and (pair? x)" +
				"              (eq? 'syntax (remvscp (car x))))" +
				"          (cons (car x)" +
				"                (cdr x)))" +
//				"          (cons (car x)" +
//				"                (lren0 (cdr x) as #t)))" +
				"        ((and (pair? x)" +
				"              (eq? 'quasisyntax (remvscp (car x))))" +
				"          (cons (car x)" +
				"                (lren0 (cdr x) as" +
				"                       (if lv lv 0))))" +
				"        ((and (pair? x)" +
				"              (eq? 'unsyntax (remvscp (car x))))" +
				"          (cons (car x)" +
				"                (lren0 (cdr x) as" +
				"                       (if (> lv 0) (- lv 1) #f))))" +
				"        ((and (pair? x)" +
				"              (eq? 'unsyntax-splicing (remvscp (car x))))" +
				"          (cons (car x)" +
				"                (lren0 (cdr x) as" +
				"                       (if (> lv 0) (- lv 1) #f))))" +
				"        ((and lv" +
				"              (pair? x)" +
				"              (memq (remvscp (car x)) '(let letrec)))" +
				"          (if (symbol? (cadr x))" +
				"              (chletn x" +
				"                      as" +
				"                      (cons" +
				"                       (cons (cadr x) (gensym))" +
				"                       (append" +
				"                        (makeasc (caddr x))" +
				"                        as))" +
				"                      lv)" +
				"              (chlet  x" +
				"                      as" +
				"                      (append (makeasc (cadr x)) as)" +
				"                      lv)))" +
				"        ((and lv" +
				"              (pair? x)" +
				"              (memq (remvscp (car x)) '(let* letrec*)))" +
				"          (chlet* x" +
				"                  as" +
				"                  (append (makeasc (cadr x)) as)" +
				"                  lv))" +
				"        ((and lv" +
				"              (pair? x)" +
				"              (memq (remvscp (car x)) '(lambda)))" +
				"          (let ((as2 (append (makeascl (cadr x)) as)))" +
				"            (cons (car x)" +
				"             (cons (lren0 (cadr x) as2 lv)" +
				"              (lren0 (cddr x) as2 lv)))))" +
				"        ((pair? x)" +
				"          (cons (lren0 (car x) as lv)" +
				"                (lren0 (cdr x) as lv)))" +
				"        ((vector? x)" +
				"          (mapv (lambda (x) (lren0 x as lv)) x))" +
				"        ((and lv (asss x as))" +
				"          (cdr (asss x as)))" +
				"        (else x)))" +
				"");
		AUX_S2.exec(
				"(define (makeasc x)" +
				"  (cond ((null? x) '())" +
				"        (else (cons (cons (caar x) (gensym))" +
				"                    (makeasc (cdr x))))))");
		AUX_S2.exec(
				"(define (makeascl x)" +
				"  (cond ((null? x) '())" +
				"        (else (cons (cons (car x) (gensym))" +
				"                    (makeascl (cdr x))))))");
		AUX_S2.exec(
				"(define (chlet x a0 as lv)" +
				"  (cons (car x)" +
				"        (cons (map (lambda (x)" +
				"                     (cons (asssif (car x) as)" +
				"                           (lren0 (cdr x) a0 lv)))" +
				"                   (cadr x))" +
				"              (lren0 (cddr x) as lv))))");
		AUX_S2.exec(
				"(define (chlet*-aux x a0 lv)" +
				"  (cond ((null? x) '())" +
				"        (else" +
				"          (cons" +
				"            (cons (remvscp (caar x))" +
				"                  (lren0 (cdar x) a0 lv))" +
				"            (chlet*-aux" +
				"              (cdr x)" +
				"              (cons" +
				"                (cons (caar x) (remvscp (caar x)))" +
				"                a0)" +
				"              lv)))))");
		AUX_S2.exec(
				"(define (chlet* x a0 as lv)" +
				"  (cons (car x)" +
				"        (cons (chlet*-aux (cadr x) a0 lv)" +
				"              (lren0 (cddr x) as lv))))");
		AUX_S2.exec(
				"(define (chletn x a0 as lv)" +
//				"(display as) (newline)" +
				"  (cons (car x)" +
				"        (cons (asssif (cadr x) as)" +
				"              (cons (map (lambda (x)" +
				"                          (cons (asssif (car x) as)" +
				"                                (lren0" +
				"                                 (cdr x) a0 lv)))" +
				"                         (caddr x))" +
				"                    (lren0 (cdddr x) as lv)))))");
		AUX_S2.exec(
				"(define (asssif x as)" +
//				"(display (asss x as)) (newline)" +
				"  (if (asss x as) (cdr (asss x as)) x))");
	}

}
