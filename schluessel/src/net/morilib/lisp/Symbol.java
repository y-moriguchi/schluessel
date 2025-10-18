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

import java.util.HashMap;
import java.util.Map;

import net.morilib.lisp.sos.LispType;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class Symbol extends Atom implements SymbolName {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/24
	 */
	public static class Namespace {

		//
		private Map<String, Symbol> flyweight =
			new HashMap<String, Symbol>();

		//
		/*package*/ Namespace() {
			// do nothing
		}

		/**
		 * 
		 * @param name
		 * @return
		 */
		public Symbol getSymbol(String name) {
			Symbol res;

			if(name == null) {
				throw new NullPointerException("Symbol");
			}

			synchronized(this) {
				res = flyweight.get(name);
				if(res == null) {
					res = new Symbol(name, this);
					flyweight.put(name, res);
				}
			}
			return res;
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2012/03/01
	 */
	public static class MacroIdAdd extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Environment env,
				LispMessage mesg) {
			macroid += SubrUtils.getSmallInt(c1a, mesg);
			if(macroid < 0) {
				macroid = Long.MAX_VALUE + macroid + 1;
			}
			return LispInteger.valueOf(macroid);
		}

	}

	//
	static final Namespace DEFAULT_NAMESPACE = new Namespace();
	static final int NO_FOLD_CASE = 0;
	static final int UPPER_FOLD_CASE = 1;
	static final int LOWER_FOLD_CASE = 2;

	//
	public static final Symbol QUOTE =
		DEFAULT_NAMESPACE.getSymbol("quote");
	public static final Symbol QUASIQUOTE =
		DEFAULT_NAMESPACE.getSymbol("quasiquote");
	public static final Symbol UNQUOTE =
		DEFAULT_NAMESPACE.getSymbol("unquote");
	public static final Symbol UNQUOTE_SPLICING =
		DEFAULT_NAMESPACE.getSymbol("unquote-splicing");

	//
	static int foldCase = NO_FOLD_CASE;

	// ----------------------------------------------------
	// symbol cannot serialize
	// ----------------------------------------------------
	//private static long genIdSeq = 1;
	//private static Map<String, Symbol> flyweight;
	private static Namespace namespace = DEFAULT_NAMESPACE;
	private static long macroid = 0;

	//
	private String name;
	//private long genId = 0;
	private Symbol replace = null;
	private boolean replaced = false;
	private Symbol macroSymbol = null;
	private Namespace ns = null;

	//
	private Symbol(String name) {
		//if(name == null) {
		//	throw new NullPointerException("Symbol");
		//}
		this.name = name;
	}

	//
	private Symbol(String name, Namespace ns) {
		this.name = name;
		this.ns   = ns;
	}

	/**
	 * 
	 * @param name
	 * @return
	 */
	public static Symbol getSymbol(String name) {
		/*Symbol res;

		if(name == null) {
			throw new NullPointerException("Symbol");
		}

		synchronized(Symbol.class) {
			if(flyweight == null) {
				//flyweight = new HashMap<String, Symbol>();
				flyweight = new WeakHashMap<String, Symbol>();
			}

			res = flyweight.get(name);
			if(res == null) {
				res = new Symbol(name);
				flyweight.put(name, res);
			}
		}
		return res;*/
		switch(foldCase) {
		case NO_FOLD_CASE:
			return namespace.getSymbol(name);
		case LOWER_FOLD_CASE:
		case UPPER_FOLD_CASE:
			return namespace.getSymbol(name.toLowerCase());
		default:  throw new RuntimeException();
		}
	}

	/**
	 * 
	 * @param name
	 * @return
	 */
	public static Symbol getSymbolWithoutFoldCase(String name) {
		return namespace.getSymbol(name);
	}

	//
	/*package*/ static Symbol newAndMark(
			Map<Symbol, Symbol> box, Symbol sym, boolean mark) {
		Symbol res;

		if(sym == null) {
			throw new NullPointerException();
		}

		if(mark) {
			res = box.get(sym);

			if(res == null) {
				//res = new Symbol(sym.name);
				res = new Symbol(null);
				//res.genId = sym.genId;
				res.replace = sym;
				res.replaced = true;
				res.macroSymbol = sym;
				box.put(sym, res);
			}
		} else {
			if(!sym.replaced) {
				res = sym;
			} else if((res = sym.replace) == null) {
				throw new NullPointerException();
			}
		}
		return res;
	}

	/**
	 * 
	 * @return
	 */
	public static Symbol gensym() {
		Symbol res = new Symbol(null);

		//res.genId = genIdSeq++;
		return res;
	}

	/**
	 * 
	 * @param sym
	 * @return
	 */
	public static Symbol encloseSymbol(Symbol sym) {
		Symbol res = new Symbol(null);

		//res.genId = genIdSeq++;
		res.macroSymbol = sym.macroSymbol;
		return res;
	}

	/**
	 * 
	 * @param sym
	 * @return
	 */
	public static Symbol getMacroSymbol(Symbol sym) {
		Symbol x = sym.getSymbol();

		return namespace.getSymbol(x.name + "#:" + macroid);
	}

	/**
	 * 
	 * @param sym
	 * @return
	 */
	public static Symbol getMacroSymbol(Symbol sym, long id) {
		Symbol x = sym.getSymbol();

		return namespace.getSymbol(x.name + "#:" + id);
	}

	/**
	 * 
	 * @param a
	 * @return
	 */
	public boolean isEqv(Atom a) {
		return equals(a);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.SymbolName#getName()
	 */
	public String getName() {
		//return (genId != 0) ? "#" + name + genId : name;
		return (name == null) ? "#:" + hashCode() : name;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isGenerated() {
		//return genId > 0;
		return name == null;
	}

	/**
	 * @return the replaced
	 */
	public boolean isReplaced() {
		return replace != null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Atom#toLispString()
	 */
	public LispString toLispString() {
		return new LispString(getName());
	}

	/**
	 * 
	 * @return
	 */
	public boolean isNormal() {
		String s = getName();

		for(int i = 0; i < s.length(); i++) {
			if(LispUtils.isReservedCharacter(s.charAt(i))) {
				return false;
			} else if(foldCase != NO_FOLD_CASE &&
					Character.isUpperCase(s.charAt(i))) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Atom#print()
	 */
	public String print() {
//		return getName();
//		return isNormal() ? getName() : "|" + getName() + "|";
		if(!isNormal()) {
			return "|" + getName() + "|";
		} else if(foldCase == NO_FOLD_CASE) {
			return getName();
		} else if(foldCase == LOWER_FOLD_CASE) {
			return getName().toLowerCase();
		} else if(foldCase == UPPER_FOLD_CASE) {
			return getName().toUpperCase();
		} else {
			throw new RuntimeException();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Atom#getResult()
	 */
	public String getResult() {
//		return getName();
		return print();
	}

	/*
	public boolean equals(Object o) {
		if(o instanceof Symbol) {
			Symbol s = (Symbol)o;

			return (name.equals(s.name) &&
					(genId == s.genId) &&
					(replaced == s.replaced));
		}
		return false;
	}

	public int hashCode() {
		int l = 17;

		l = 37 * l + name.hashCode();
		l = 37 * l + (int)(l ^ (l >>> 32));
		return l;
	}
	*/

	//
	/*package*/ Symbol getEnclosedSymbol() {
		Symbol sym = this;

		while(sym.macroSymbol != null) {
			sym = sym.macroSymbol;
		}
		return sym;
		//return macroSymbol;
	}

	/*package*/ /*Symbol getReplacedRoot() {
		Symbol sym = this;

		while(sym.replace != null) {
			sym = sym.replace;
		}
		return sym;
	}*/

	/**
	 * 
	 */
	public boolean isMacroBound() {
		return name != null && name.indexOf("#:") >= 0;
	}

	/**
	 * 
	 * @return
	 */
	public long getMacroId() {
		int x = name.indexOf("#:");

		return (x < 0) ? x : Long.parseLong(name.substring(x + 2));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toString()
	 */
	public String toString() {
		return ("Symbol:" + getName() + ":" +
				System.identityHashCode(this));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#isTypeSymbol()
	 */
	public boolean isTypeSymbol() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.SymbolName#getSymbol()
	 */
	public Symbol getSymbol() {
//		return this;
		int x = (name == null) ? -1 : name.indexOf("#:");

		return (x < 0) ? this : Symbol.getSymbol(name.substring(0, x));
	}

	/**
	 * 
	 * @return
	 */
	public Namespace getNamespace() {
		return ns;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getType()
	 */
	@Override
	public LispType getType() {
		return LispType.SYMBOL;
	}

}
