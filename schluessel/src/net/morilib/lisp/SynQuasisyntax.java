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
import java.util.List;

public class SynQuasisyntax extends SynQuasiquote {
	
	/*package*/ static final Symbol QUASISYNTAX =
		Symbol.getSymbol("quasisyntax");
	
	/*package*/ static final Symbol UNSYNTAX =
		Symbol.getSymbol("unsyntax");
	
	/*package*/ static final Symbol UNSYNTAX_SPLICING =
		Symbol.getSymbol("unsyntax-splicing");
	
	private static class Wrap extends Datum
	implements SyntaxUtils.SafeWrap {
		private Datum wrapee;
		
		private Wrap(Datum w) {
			wrapee = w;
		}
		
		public Datum getWrapee() {
			return wrapee;
		}
	}
	
	
	/*package*/ boolean equalsQuasiquote(Datum d) {
		return SyntaxUtils.equalsReserved(QUASISYNTAX, d);
	}
	
	/*package*/ boolean equalsUnquote(Datum d) {
		return SyntaxUtils.equalsReserved(UNSYNTAX, d);
	}
	
	/*package*/ boolean equalsUnquoteSplicing(Datum d) {
		return SyntaxUtils.equalsReserved(UNSYNTAX_SPLICING, d);
	}
	
	/*package*/ LispException error(LispMessage mesg) {
		return mesg.getError("err.quasisyntax.malform");
	}
	
	private static Datum getCaar(Cons c) {
		if(c.getCar() instanceof Cons) {
			return ((Cons)c.getCar()).getCar();
		} else {
			return null;
		}
	}
	
	
	public String toString() {
		return "Syntax:quasiquote";
	}
	
	/*private Datum wrap2(
			Datum d,
			Environment env,
			LispCompiler comp,
			Environment ienv,
			LispMessage mesg,
			int ttype) {
		if(d instanceof Cons) {
			Cons c0 = (Cons)d;
			
			if(equalsQuasiquote(c0.getCar())) {
				Cons r0 = new Cons();
				
				r0.setCar(c0.getCar());
				r0.setCdr(extract1(
						0, c0.getCdr(), env, comp, ienv, mesg, 2));
				return r0;
			} else {
				ConsIterator itr = new ConsIterator(d);
				ConsListBuilder bld = new ConsListBuilder();
				
				while(itr.hasNext()) {
					Datum d2 = itr.next();
					
					bld.append(wrap2(
							d2, env, comp, ienv, mesg, 2));
				}
				return bld.get(wrap2(
						itr.getTerminal(),
						env, comp, ienv, mesg, 2));
			}
		} else {
			return d;
		}
	}*/
	
	private Datum quoteE1(
			int level,
			Cons c,
			Environment env,
			LispCompiler comp,
			Environment ienv,
			LispMessage mesg,
			int ttype) {
		// (car (cdr c))がないときは内部エラー
		Cons c2 = (Cons)c.getCdr();
		Cons res = new Cons();
		Cons r2  = new Cons();
		
		res.setCar(c.getCar());
		res.setCdr(r2);
		r2.setCar(extract1(
				level, c2.getCar(), env,
				comp, ienv, mesg, ttype));
		
		return res;
	}
	
	private Datum extract1(
			int level,
			Datum body,
			Environment env,
			LispCompiler comp,
			Environment ienv,
			LispMessage mesg,
			int ttype) {
		if(body instanceof Cons) {
			Cons c = (Cons)body;
			
			if(equalsQuasiquote(c.getCar())) {
				return quoteE1(
						level + 1, c, env, comp, ienv, mesg, ttype);
			} else if(equalsUnquote(c.getCar())) {
				if(level > 0) {
					return quoteE1(
							level - 1, c, env, comp, ienv, mesg, ttype);
				} else if(c.getCdr() instanceof Cons) {
					// (car (cdr c))がないときは内部エラー
//					Cons c2 = (Cons)c.getCdr();
//					Cons r1 = new Cons();
//					Cons r2 = new Cons();
//					
//					r1.setCar(c.getCar());
//					r1.setCdr(r2);
//					r2.setCar(comp.replaceLocalVals(
//							c2.getCar(), env, ienv, false, ttype));
//					return r1;
					//return new Wrap(c);
					
					//Datum r3 = wrap2(
					//		c, env, comp, ienv, mesg, ttype);
					//return (ttype == 1) ? new Wrap(r3) : r3;
					//return new Wrap(r3);
					//return r3;
					return c;
				} else {
					throw error(mesg);
				}
			} else if(equalsUnquoteSplicing(c.getCar())) {
				if(level > 0) {
					return quoteE1(
							level - 1, c, env, comp, ienv, mesg, ttype);
				} else if(c.getCdr() instanceof Cons) {
					// (car (cdr c))がないときは内部エラー
//					Cons c2 = (Cons)c.getCdr();
//					Cons r1 = new Cons();
//					Cons r2 = new Cons();
//					
//					r1.setCar(c.getCar());
//					r1.setCdr(r2);
//					r2.setCar(comp.replaceLocalVals(
//							c2.getCar(), env, ienv, false, ttype));
//					return r1;
					//return new Wrap(c);
					
					//Datum r3 = wrap2(
					//		c, env, comp, ienv, mesg, ttype);
					//return (ttype == 1) ? new Wrap(r3) : r3;
					//return new Wrap(r3);
					//return r3;
					return c;
				} else {
					throw error(mesg);
				}
			} else {
				List<Datum> lst = new ArrayList<Datum>();
				
				while(true) {
					Datum caar = getCaar(c);
					
					lst.add(extract1(
							level, c.getCar(), env,
							comp, ienv, mesg, ttype));
					
					if(c.getCdr() instanceof Cons) {
						Cons c2 = (Cons)c.getCdr();
						Datum d2 = c2.getCar();
						
						if(equalsUnquote(d2)) {
							// `(a b . ,(expr))
							Datum dz = extract1(
									level, c.getCdr(),
									env, comp, ienv, mesg, ttype);
							
							return LispUtils.listToCons(lst, dz);
							//break;
						} else if(equalsUnquoteSplicing(d2)) {
							// `(a b . ,@(expr))
							throw error(mesg);
						} else if(level == 0 &&
								equalsUnquoteSplicing(caar)) {
							// `(a b ,@(expr) d ...)のとき
							c = (Cons)c.getCdr();
						} else {
							c = (Cons)c.getCdr();
						}
					} else if(c.getCdr() == Nil.NIL) {
						return LispUtils.listToCons(lst);
					} else {
						Datum dd = extract1(
								level, c.getCdr(),
								env, comp, ienv, mesg, ttype);
						
						return LispUtils.listToCons(lst, dd);
						//break;
					}
				}
			}
		} else if(body instanceof LispVector) {
			List<Datum> lst = new ArrayList<Datum>();
			LispVector v = (LispVector)body;
			
			for(int i = 0; i < v.size(); i++) {
				lst.add(extract1(
						level, v.get(i), env, comp, ienv, mesg, ttype));
			}
			return new LispVector(lst);
		} else {
			//return body;
			return comp.replaceLocalVals(body, env, ienv, false, ttype);
		}
	}
	
	private Datum extract1Only(
			int level,
			Datum body,
			Environment env,
			LispCompiler comp,
			Environment ienv,
			LispMessage mesg,
			int ttype) {
		if(body instanceof Cons) {
			Cons c = (Cons)body;
			
			if(equalsQuasiquote(c.getCar())) {
				return extract1Only(
						level + 1, c, env, comp, ienv, mesg, ttype);
			} else if(equalsUnquote(c.getCar())) {
				if(level > 0) {
					return extract1Only(
							level - 1, c, env, comp, ienv, mesg, ttype);
				} else if(c.getCdr() instanceof Cons) {
					return comp.replaceLocalValsOnly(
									c, env, ienv, false, ttype);
				} else {
					throw error(mesg);
				}
			} else if(equalsUnquoteSplicing(c.getCar())) {
				if(level > 0) {
					return quoteE1(
							level - 1, c, env, comp, ienv, mesg, ttype);
				} else if(c.getCdr() instanceof Cons) {
					return comp.replaceLocalValsOnly(
							c, env, ienv, false, ttype);
				} else {
					throw error(mesg);
				}
			} else {
				Cons res = new Cons();
				
				res.setCar(extract1Only(
						level,
						c.getCar(), env, comp, ienv, mesg, ttype));
				res.setCdr(extract1Only(
						level,
						c.getCdr(), env, comp, ienv, mesg, ttype));
				return res;
			}
		} else if(body instanceof LispVector) {
			List<Datum> lst = new ArrayList<Datum>();
			LispVector v = (LispVector)body;
			
			for(int i = 0; i < v.size(); i++) {
				lst.add(extract1Only(
						level, v.get(i), env, comp, ienv, mesg, ttype));
			}
			return new LispVector(lst);
		} else {
			//return body;
			return comp.replaceLocalVals(body, env, ienv, false, ttype);
		}
	}
	
	
	/*package*/ Datum replaceLocalVals(
			Datum body,
			Environment env,
			LispCompiler comp,
			Environment ienv,
			LispMessage mesg,
			boolean toplv,
			int ttype) {
		if(ttype == 0) {
			Cons r0 = new Cons();
			Cons r1 = new Cons();
			//Cons r2 = new Cons();
			
			r0.setCar(this);
			r0.setCdr(r1);
			//r1.setCar(r2);
			r1.setCar(extract1Only(
					0, body, env, comp, ienv, mesg, ttype));
			return new Wrap(r0);
			//return body;
		} else if(ttype == 1 || ttype == 2) {
			if(body instanceof Cons) {
				Cons res = new Cons();
				Datum b1;
				
				b1 = extract1(
						0, ((Cons)body).getCar(), env,
						comp, ienv, mesg, ttype);
				//b1 = AUX_S.call("letren", b1);
				res.setCar(b1);
				return res;
//				return comp.replaceLocalVals(
//						body, env, ienv, toplv, ttype);
			} else {
				throw error(mesg);
			}
			//return comp.replaceLocalVals(
			//		body, env, ienv, toplv, ttype);
		} else {
			throw new RuntimeException();
		}
		//return comp.replaceLocalVals(body, env, ienv, toplv);
	}
	
}
