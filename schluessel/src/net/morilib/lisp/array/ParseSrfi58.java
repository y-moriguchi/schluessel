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
package net.morilib.lisp.array;

import java.util.HashMap;
import java.util.Map;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispVector;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.primitive.IntegerArrayVector;
import net.morilib.util.primitive.IntegerVector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/28
 */
public class ParseSrfi58 extends BinaryArgs {

	//
	private static final Map<String, LispArrayPrototype> _PROTOTYPES;
	private static final LispVector ANYVEC =
		new LispVector(Undef.UNDEF);

	//
	static {
		_PROTOTYPES = new HashMap<String, LispArrayPrototype>();
		_PROTOTYPES.put("floC128b", new LispArrayC64.Prototype(0, 0));
		_PROTOTYPES.put("floC64b",  new LispArrayC64.Prototype(0, 0));
		_PROTOTYPES.put("floC32b",  new LispArrayC32.Prototype(0, 0));
		_PROTOTYPES.put("floC16b",  new LispArrayC32.Prototype(0, 0));
		_PROTOTYPES.put("floR128b", new LispArrayR64.Prototype(0));
		_PROTOTYPES.put("floR64b",  new LispArrayR64.Prototype(0));
		_PROTOTYPES.put("floR32b",  new LispArrayR32.Prototype(0));
		_PROTOTYPES.put("floR16b",  new LispArrayR32.Prototype(0));
		_PROTOTYPES.put("floQ128d", new LispVector(Undef.UNDEF));
		_PROTOTYPES.put("floQ64d",  new LispVector(Undef.UNDEF));
		_PROTOTYPES.put("floQ32d",  new LispVector(Undef.UNDEF));
		_PROTOTYPES.put("fixZ64b",  new LispArrayS64.Prototype(0));
		_PROTOTYPES.put("fixZ32b",  new LispArrayS32.Prototype(0));
		_PROTOTYPES.put("fixZ16b",  new LispArrayS16.Prototype(
				(short)0));
		_PROTOTYPES.put("fixZ8b",   new LispArrayS8.Prototype(
				(byte)0));
		_PROTOTYPES.put("fixN64b",  new LispArrayU64.Prototype(0));
		_PROTOTYPES.put("fixN32b",  new LispArrayU32.Prototype(0));
		_PROTOTYPES.put("fixN16b",  new LispArrayU16.Prototype(
				(short)0));
		_PROTOTYPES.put("fixN8b",   new LispArrayU8.Prototype(
				(byte)0));
		_PROTOTYPES.put("bool",     new LispArrayT1.Prototype(false));
		_PROTOTYPES.put("char",     new LispString("\0"));
	}

	//
	private static ILispArray makearray(int[] dims, String s, int p,
			LispMessage mesg) {
		if(p < s.length()) {
			LispArrayPrototype t = _PROTOTYPES.get(s.substring(p));

			if(t == null) {
				throw mesg.getError(
						"err.srfi58.notation.specifier.invalid", s);
			}
			return t.makeArray(dims);
		} else {
			return ANYVEC.makeArray(dims);
		}
	}

	//
	private static ILispArray makearray(int rank, String s, Datum d,
			int p, LispMessage mesg) {
		int[] is = ListToArray.getDimensions(d, mesg);

		if(rank >= 0 && is.length != rank) {
			throw mesg.getError(
					"err.srfi58.notation.mismatch.rank", s);
		}
		return makearray(is, s, p, mesg);
	}

	//
	/*package*/ static ILispArray parseNotation(String s, Datum d,
			LispMessage mesg) {
		int p = 0, stat = 0;
		StringBuilder b = new StringBuilder();
		int rank = -1;
		IntegerVector dims = null;

		for(; p <= s.length(); p++) {
			char c = (p < s.length()) ? s.charAt(p) : '\0';

			switch(stat) {
			case 0:
				if(c >= '0' && c <= '9') {
					b.append(c);
					stat = 1;
				} else if(c == 'A') {
					stat = 2;
				} else {
					throw mesg.getError(
							"err.srfi58.notation.syntaxerror", s);
				}
				break;
			case 1:
				if(c >= '0' && c <= '9') {
					b.append(c);
				} else if(c == 'A') {
					rank = Integer.parseInt(b.toString());
					b = new StringBuilder();
					stat = 2;
				} else if(c == '*') {
					if(dims == null) {
						dims = new IntegerArrayVector();
					}
					dims.add(Integer.parseInt(b.toString()));
					b = new StringBuilder();
					stat = 2;
				} else if(c == ':' || c == '\0') {
					if(dims == null) {
						dims = new IntegerArrayVector();
					}
					dims.add(Integer.parseInt(b.toString()));
					return makearray(dims.toIntArray(), s, p + 1,
							mesg);
				} else {
					throw mesg.getError(
							"err.srfi58.notation.syntaxerror", s);
				}
				break;
			case 2:
				if(c >= '0' && c <= '9') {
					b.append(c);
					stat = 3;
				} else if(c == ':' || c == '\0') {
					if(dims != null && rank != dims.size()) {
						throw mesg.getError(
								"err.srfi58.notation.mismatch.rank",
								s);
					} else if(dims == null) {
						return makearray(rank, s, d, p + 1, mesg);
					} else {
						return makearray(dims.toIntArray(), s, p + 1,
								mesg);
					}
				} else {
					throw mesg.getError(
							"err.srfi58.notation.syntaxerror", s);
				}
				break;
			case 3:
				if(c >= '0' && c <= '9') {
					b.append(c);
					stat = 3;
				} else if(c == '*') {
					if(dims == null) {
						dims = new IntegerArrayVector();
					}
					dims.add(Integer.parseInt(b.toString()));
					b = new StringBuilder();
					stat = 2;
				} else if(c == ':' || c == '\0') {
					if(dims == null) {
						dims = new IntegerArrayVector();
					}
					dims.add(Integer.parseInt(b.toString()));
					if(rank >= 0 && rank != dims.size()) {
						throw mesg.getError(
								"err.srfi58.notation.mismatch.rank",
								s);
					}
					return makearray(dims.toIntArray(), s, p + 1,
							mesg);
				} else {
					throw mesg.getError(
							"err.srfi58.notation.syntaxerror", s);
				}
				break;
			default:
				throw new RuntimeException();
			}
		}
		throw new RuntimeException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		String s = SubrUtils.getString(c1a, mesg);
		ILispArray a = parseNotation(s, c2a, mesg);

		try {
			return (Datum)ListToArray.setListToArray(a, c2a);
		} catch(InvalidDimensionException e) {
			throw mesg.getError("err.srfi25.dimension.invalid", c1a);
		} catch(IndexOutOfBoundsException e) {
			throw mesg.getError("err.range.invalid");
		} catch(ClassCastException e) {
			throw mesg.getError("err.srfi25.typemismatch");
		} catch(ValueOutOfBoundsException e) {
			throw mesg.getError("err.srfi47.valueoutofrange",
					e.getMessage());
		}
	}

}
