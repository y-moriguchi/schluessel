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
import java.io.PushbackReader;
import java.io.Reader;
import java.util.logging.Logger;

import net.morilib.util.io.CharUnreadBuffer;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
/*package*/ final class IntLispUtils {

	//
	private IntLispUtils() {
		// do nothing
	}

	//
	/*package*/ static Datum car(Datum d, LispMessage mesg) {
		if(d instanceof Cons) {
			return ((Cons)d).getCar();
		}
		throw mesg.getError("err.require.pair"); 
	}

	//
	/*package*/ static Datum cdr(Datum d, LispMessage mesg) {
		if(d instanceof Cons) {
			return ((Cons)d).getCdr();
		}
		throw mesg.getError("err.require.pair"); 
	}

	//
	/*package*/ static Datum caar(Datum d, LispMessage mesg) {
		if(d instanceof Cons) {
			Datum c0 = ((Cons)d).getCar();

			if(c0 instanceof Cons) {
				return ((Cons)c0).getCar();
			}
		}
		throw mesg.getError("err.require.pair"); 
	}

	//
	/*package*/ static Datum cdar(Datum d, LispMessage mesg) {
		if(d instanceof Cons) {
			Datum c0 = ((Cons)d).getCar();

			if(c0 instanceof Cons) {
				return ((Cons)c0).getCdr();
			}
		}
		throw mesg.getError("err.require.pair"); 
	}

	//
	/*package*/ static Datum cadr(Datum d, LispMessage mesg) {
		if(d instanceof Cons) {
			Datum c0 = ((Cons)d).getCdr();

			if(c0 instanceof Cons) {
				return ((Cons)c0).getCar();
			}
		}
		throw mesg.getError("err.require.pair"); 
	}

	//
	/*package*/ static Datum cddr(Datum d, LispMessage mesg) {
		if(d instanceof Cons) {
			Datum c0 = ((Cons)d).getCdr();

			if(c0 instanceof Cons) {
				return ((Cons)c0).getCdr();
			}
		}
		throw mesg.getError("err.require.pair"); 
	}

	//
	static Datum cadadr(Datum d, LispMessage mesg) {
		Datum e = d;

		if(!(e instanceof Cons)) {
			throw mesg.getError("err.require.pair"); 
		} else if(!((e = ((Cons)e).getCdr()) instanceof Cons)) {
			throw mesg.getError("err.require.pair"); 
		} else if(!((e = ((Cons)e).getCar()) instanceof Cons)) {
			throw mesg.getError("err.require.pair"); 
		} else if(!((e = ((Cons)e).getCdr()) instanceof Cons)) {
			throw mesg.getError("err.require.pair"); 
		} else {
			return ((Cons)e).getCar();
		}
	}

	//
	static Datum cadaddr(Datum d, LispMessage mesg) {
		Datum e = d;

		if(!(e instanceof Cons)) {
			throw mesg.getError("err.require.pair"); 
		} else if(!((e = ((Cons)e).getCdr()) instanceof Cons)) {
			throw mesg.getError("err.require.pair"); 
		} else if(!((e = ((Cons)e).getCdr()) instanceof Cons)) {
			throw mesg.getError("err.require.pair"); 
		} else if(!((e = ((Cons)e).getCar()) instanceof Cons)) {
			throw mesg.getError("err.require.pair"); 
		} else if(!((e = ((Cons)e).getCdr()) instanceof Cons)) {
			throw mesg.getError("err.require.pair"); 
		} else {
			return ((Cons)e).getCar();
		}
	}

	//
	static Datum caddaaddr(Datum d, LispMessage mesg) {
		Datum e = d;

		if(!(e instanceof Cons)) {
			throw mesg.getError("err.require.pair"); 
		} else if(!((e = ((Cons)e).getCdr()) instanceof Cons)) {
			throw mesg.getError("err.require.pair"); 
		} else if(!((e = ((Cons)e).getCdr()) instanceof Cons)) {
			throw mesg.getError("err.require.pair"); 
		} else if(!((e = ((Cons)e).getCar()) instanceof Cons)) {
			throw mesg.getError("err.require.pair"); 
		} else if(!((e = ((Cons)e).getCar()) instanceof Cons)) {
			throw mesg.getError("err.require.pair"); 
		} else if(!((e = ((Cons)e).getCdr()) instanceof Cons)) {
			throw mesg.getError("err.require.pair"); 
		} else if(!((e = ((Cons)e).getCdr()) instanceof Cons)) {
			throw mesg.getError("err.require.pair"); 
		} else {
			return ((Cons)e).getCar();
		}
	}

	/*package*/ /*static Cons extractLambda(
			Datum bcdr, Datum bcar) {
		if(bcdr instanceof Cons) {
			Cons  c0 = (Cons)bcdr;
			Datum d0 = c0.getCar();

			if(d0 instanceof Cons &&
					((Cons)d0).getCar().equals(LAMBDA)) {
				Cons c1 = (Cons)d0;

				if(c1.getCdr() instanceof Cons) {
					Cons c2 = (Cons)c1.getCdr();

					// error check
					return new Cons(bcar, c2.getCar());
				}
			}
		}
		return null;
	}*/

	/*package*/ /*static Datum extractLambdaList(Datum bcdr) {
		if(bcdr instanceof Cons) {
			Cons  c0 = (Cons)bcdr;
			Datum d0 = c0.getCar();

			if(d0 instanceof Cons &&
					((Cons)d0).getCar().equals(LAMBDA)) {
				Cons c1 = (Cons)d0;

				if(c1.getCdr() instanceof Cons) {
					Cons c2 = (Cons)c1.getCdr();

					// error check
					return c2.getCdr();
				}
			}
		}
		return null;
	}*/

	//
	/*package*/ static void bindLocal(
			Datum prms, Datum bcdr, Environment env, LispMessage mesg) {
		while(true) {
			if(prms == Nil.NIL) {
				if(bcdr != Nil.NIL) {
					throw mesg.getError("err.parameter.insufficient");
				}
				break;
			} else if(prms instanceof Atom) {
				if(!(prms instanceof Symbol)) {
					throw mesg.getError("err.symbol", prms);
				}
				env.bindDatum(prms, bcdr);
				break;
			} else if(!(bcdr instanceof Cons)) {
				//throw new LispException("insufficient parameter");
				throw mesg.getError("err.parameter.insufficient");
			} else if(prms instanceof Cons) {
				Datum a1 = ((Cons)prms).getCar();
				Datum a2 = ((Cons)bcdr).getCar();

				if(a1 instanceof SymbolName) {
					env.bindDatum(a1, a2);
				//} else if(a1 instanceof SymbolScope) {
					//SymbolScope ssp = (SymbolScope)a1;

					////if(ssp.isCaptured()) {
					////	env.bindDatum(ssp.getSymbol(), a2);
					////} else {
					////	throw mesg.getError("err.symbol", a1);
					////}
					//env.bindDatum(ssp.getSymbol(), a2);
				} else {
					throw mesg.getError("err.symbol", a1);
				}

				prms = ((Cons)prms).getCdr();
				bcdr = ((Cons)bcdr).getCdr();
			} else {
				//throw new LispException("Invalid type");
				throw mesg.getError("err.type.insufficient");
			}
		}
	}

	//
	/*package*/ static Datum findListEqv(
			Datum k, Datum lst, LispMessage mesg) {
		Datum d = lst;

		while(d != Nil.NIL) {
			if(lst instanceof Cons) {
				Cons c1 = (Cons)d;

				if(c1.getCar() instanceof Symbol) {
					Symbol s = (Symbol)c1.getCar();
					Symbol r = s.getEnclosedSymbol();

					if(r == null && k == s) {
						return k;
					} else if(k == r) {
						// symbol in case
						return k;
					}
				} else if(k.isEqv(c1.getCar())) {
					return k;
				}
				d = c1.getCdr();
			} else {
				throw mesg.getError("err.list");
			}
		}
		return LispBoolean.FALSE;
	}

	//
	/*package*/ static boolean isSymbolName(Datum d) {
		return (d instanceof SymbolName);
	}

	//
	/*package*/ static void timelog(Logger _log, String str, long bef) {
		long dt = System.currentTimeMillis() - bef;
		String msec = ("000" + (dt % 1000));

		msec = msec.substring(msec.length() - 3);
		_log.fine(str + (dt / 1000) + "." + msec + "sec");
	}

	//
	/*package*/ static void timelogFiner(
			Logger _log, String str, long bef) {
		long dt = System.currentTimeMillis() - bef;
		String msec = ("000" + (dt % 1000));

		msec = msec.substring(msec.length() - 3);
		_log.finer(str + (dt / 1000) + "." + msec + "sec");
	}

	//
	static boolean loadEager = false;

	//
	/*package*/ static void loadJavaSubr(
			Environment env, Symbol k, String v) {
		//try {
		//	Class<?> cls = Class.forName(v);
		//	Datum ins = (Datum)cls.newInstance();
		//	
		//	if(ins instanceof Subr) {
		//		((Subr)ins).symbolName = k.getName();
		//	} else if(ins instanceof Syntax) {
		//		((Syntax)ins).symbolName = k.getName();
		//	}
		//	env.bindDatum(k, ins);
		//} catch (ClassNotFoundException e) {
		//	throw new RuntimeException(e);
		//} catch (InstantiationException e) {
		//	throw new RuntimeException(e);
		//} catch (IllegalAccessException e) {
		//	throw new RuntimeException(e);
		//}
		if(loadEager) {
			env.bindDatum(k, getJavaSubr(k, v));
		} else {
			env.bindDatum(k, new LispLazySubrSyn(env, k, v));
		}
	}

	//
	/*package*/ static Datum getJavaSubr(Symbol k, String v) {
		try {
			Class<?> cls = Class.forName(v);
			Datum ins = (Datum)cls.newInstance();

			if(ins instanceof Subr) {
				((Subr)ins).symbolName = k.getName();
			} else if(ins instanceof Syntax) {
				((Syntax)ins).symbolName = k.getName();
			}
			return ins;
		} catch (ClassNotFoundException e) {
			throw new RuntimeException(e);
		} catch (InstantiationException e) {
			throw new RuntimeException(e);
		} catch (IllegalAccessException e) {
			throw new RuntimeException(e);
		}
	}

	//
	/*package*/ static void loadJavaSubr(Environment env, Symbol k,
			String v, ClassLoader loader) {
		env.bindDatum(k, getJavaSubr(k, v, loader));
	}

	//
	/*package*/ static Datum getJavaSubr(Symbol k, String v,
			ClassLoader loader) {
		try {
			Class<?> cls = loader.loadClass(v);
			Datum ins = (Datum)cls.newInstance();

			if(ins instanceof Subr) {
				((Subr)ins).symbolName = k.getName();
			} else if(ins instanceof Syntax) {
				((Syntax)ins).symbolName = k.getName();
			}
			return ins;
		} catch (ClassNotFoundException e) {
			throw new RuntimeException(e);
		} catch (InstantiationException e) {
			throw new RuntimeException(e);
		} catch (IllegalAccessException e) {
			throw new RuntimeException(e);
		}
	}

	//
	/*package*/ static boolean skipShebang(
			Reader pb,
			CharUnreadBuffer ub) throws IOException {
		int c, st = 0;

		while(true) {
			if((c = pb.read()) < 0) {
				return false;
			}

			switch(st) {
			case 0:  // #
				if(c != '#') {
					ub.unread((char)c);
					return true;
				}
				st = 1;
				break;
			case 1: // !
				if(c != '!') {
					ub.unread('#');
					ub.unread((char)c);
					return true;
				}
				st = 2;
				break;
			default:
				if(c == '\n') {
					return true;
				}
				break;
			}
		}
	}

	//
	/*package*/ static boolean skipShebang(
			PushbackReader pb) throws IOException {
		int c, st = 0;

		while(true) {
			if((c = pb.read()) < 0) {
				return false;
			}

			switch(st) {
			case 0:  // #
				if(c != '#') {
					pb.unread((char)c);
					return true;
				}
				st = 1;
				break;
			case 1: // !
				if(c != '!') {
					pb.unread('#');
					pb.unread((char)c);
					return true;
				}
				st = 2;
				break;
			default:
				if(c == '\n') {
					return true;
				}
				break;
			}
		}
	}

}
