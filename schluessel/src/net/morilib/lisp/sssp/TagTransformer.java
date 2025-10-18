/*
 * Copyright 2009-2010 Yuichiro Moriguchi
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
package net.morilib.lisp.sssp;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.LispString;
import net.morilib.lisp.Symbol;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/12/30
 */
public abstract class TagTransformer {

	/**
	 * 
	 */
	public static final TagTransformer STRING = new TagTransformer() {

		@Override
		public Datum transform(String s) {
			return new LispString(s);
		}

	};

	/**
	 * 
	 */
	public static final TagTransformer INTEGER = new TagTransformer() {

		@Override
		public Datum transform(String s) {
			try {
				return LispInteger.valueOf(new BigInteger(s));
			} catch(NumberFormatException e) {
				return LispBoolean.FALSE;
			}
		}

	};

	/**
	 * 
	 */
	public static final TagTransformer BOOLEAN = new TagTransformer() {

		@Override
		public Datum transform(String s) {
			return LispBoolean.getInstance(
					s.equalsIgnoreCase("true") ||
					s.equalsIgnoreCase("on") ||
					s.equalsIgnoreCase("yes"));
		}

	};

	/**
	 * 
	 */
	public static final TagTransformer NUMBER = new TagTransformer() {

		@Override
		public Datum transform(String s) {
			Datum d = LispNumber.parse(s, 10);

			return (d == null) ? LispBoolean.FALSE : d;
		}

	};

	/**
	 * 
	 */
	public static final TagTransformer SYMBOL = new TagTransformer() {

		@Override
		public Datum transform(String s) {
			return Symbol.getSymbol(s);
		}

	};

	//
	private static final Map<Datum, TagTransformer> TAGS;

	//
	static {
		TAGS = new HashMap<Datum, TagTransformer>();
		TAGS.put(Symbol.getSymbol("string"),  STRING);
		TAGS.put(Symbol.getSymbol("symbol"),  SYMBOL);
		TAGS.put(Symbol.getSymbol("integer"), INTEGER);
		TAGS.put(Symbol.getSymbol("boolean"), BOOLEAN);
		TAGS.put(Symbol.getSymbol("number"),  NUMBER);
	}

	/**
	 * 
	 */
	protected TagTransformer() { }

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static TagTransformer getInstance(Datum s) {
		return TAGS.get(s);
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public abstract Datum transform(String s);

}
