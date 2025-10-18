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
package net.morilib.lisp.charset;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispCharacter;
import net.morilib.lisp.topology.AbstractLispTopology;
import net.morilib.lisp.topology.ILispOrderedSet;
import net.morilib.lisp.topology.ILispTopology;
import net.morilib.lisp.topology.LispCardinality;
import net.morilib.range.integer.AbstractIntRange;
import net.morilib.range.integer.IntCharSets;
import net.morilib.range.integer.IntInterval;
import net.morilib.range.integer.IntRange;
import net.morilib.util.MutablePair;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/09
 */
public class LispCharSet extends AbstractLispTopology
implements ILispOrderedSet, java.io.Serializable {

	//
	private static final Map<String, LispCharSet> _RANGES;

	/**
	 * 
	 */
	public static final LispCharSet UNIVERSAL =
		new LispCharSet(IntCharSets.ALL_CHAR);

	/**
	 * 
	 */
	public static final LispCharSet LOWER_CASE;

	/**
	 * 
	 */
	public static final LispCharSet UPPER_CASE;

	/**
	 * 
	 */
	public static final LispCharSet TITLE_CASE;

	/**
	 * 
	 */
	public static final LispCharSet LETTER;

	/**
	 * 
	 */
	public static final LispCharSet DIGIT;

	/**
	 * 
	 */
	public static final LispCharSet LETTER_PLUS_DIGIT;

	/**
	 * 
	 */
	public static final LispCharSet GRAPHIC;

	/**
	 * 
	 */
	public static final LispCharSet PRINTING;

	/**
	 * 
	 */
	public static final LispCharSet WHITESPACE;

	/**
	 * 
	 */
	public static final LispCharSet ISO_CONTROL;

	/**
	 * 
	 */
	public static final LispCharSet PUNCTATION;

	/**
	 * 
	 */
	public static final LispCharSet SYMBOL;

	/**
	 * 
	 */
	public static final LispCharSet HEX_DIGIT;

	/**
	 * 
	 */
	public static final LispCharSet BLANK;

	/**
	 * 
	 */
	public static final LispCharSet ASCII;

	/**
	 * 
	 */
	public static final LispCharSet EMPTY;

	/**
	 * 
	 */
	public static final LispCharSet FULL;

	//
	static {
		InputStream ins = LispCharSet.class.getResourceAsStream(
				"/net/morilib/lisp/charset/charset2.txt");

		_RANGES = new HashMap<String, LispCharSet>();
		try {
			final MutablePair<String, SortedSet<IntInterval>> mt;

			mt = new MutablePair<String, SortedSet<IntInterval>>();
			CharSetPropertyParser.parse(ins,
					new CharSetPropertyHandler() {

				public void singleChar(int ch) {
					mt.getB().add(getintv(ch, ch));
				}
				
				public void rangedChar(int cb, int ce) {
					mt.getB().add(getintv(cb, ce));
				}
				
				public void charsetName(String name) {
					if(mt.getA() != null) {
						IntRange rr;

						rr = AbstractIntRange.makeRange(mt.getB());
						_RANGES.put(mt.getA(), new LispCharSet(rr));
					}
					mt.setA(name);
					mt.setB(new TreeSet<IntInterval>());
				}
			});

			LOWER_CASE        = _RANGES.get("char-set:lower-case");
			UPPER_CASE        = _RANGES.get("char-set:upper-case");
			TITLE_CASE        = _RANGES.get("char-set:title-case");
			LETTER            = _RANGES.get("char-set:letter");
			DIGIT             = _RANGES.get("char-set:digit");
			LETTER_PLUS_DIGIT = _RANGES.get("char-set:letter+digit");
			GRAPHIC           = _RANGES.get("char-set:graphic");
			PRINTING          = _RANGES.get("char-set:printing");
			WHITESPACE        = _RANGES.get("char-set:whitespace");
			ISO_CONTROL       = _RANGES.get("char-set:iso-control");
			PUNCTATION        = _RANGES.get("char-set:punctuation");
			SYMBOL            = _RANGES.get("char-set:symbol");
			HEX_DIGIT         = _RANGES.get("char-set:hex-digit");
			BLANK             = _RANGES.get("char-set:blank");
			ASCII             = _RANGES.get("char-set:ascii");
			EMPTY             = _RANGES.get("char-set:empty");
			FULL              = _RANGES.get("char-set:full");
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	//
	/*package*/ IntRange charset;

	/**
	 * 
	 * @param cs
	 */
	public LispCharSet(char... cs) {
		charset = IntRange.O;

		for(char c : cs) {
			charset = charset.join(new IntInterval(c, c));
		}
	}

	/**
	 * 
	 * @param cs
	 */
	public LispCharSet(int... cs) {
		charset = IntRange.O;

		for(int c : cs) {
			charset = charset.join(new IntInterval(c, c));
		}
	}

	/**
	 * 
	 * @param cs
	 */
	public LispCharSet(CharSequence cs) {
		charset = IntRange.O;
		for(int i = 0; i < cs.length(); i++) {
			int c = cs.charAt(i);

			charset = charset.join(new IntInterval(c, c));
		}
	}

	/**
	 * 
	 * @param s
	 */
	public LispCharSet(String s) {
		charset = IntRange.O;
		for(int i = 0; i < s.length();) {
			int c = s.codePointAt(i);

			charset = charset.join(new IntInterval(c, c));
			i += (c > Character.MAX_VALUE) ? 2 : 1;
		}
	}

	/**
	 * 
	 * @param charset
	 */
	public LispCharSet(IntRange charset) {
		this.charset = charset;
	}

	/**
	 * 
	 * @param cs
	 */
	public LispCharSet(LispCharSet cs) {
		this.charset = cs.charset;
	}

	//
	private static IntInterval getintv(int ch, int ch2) {
		return new IntInterval(ch, ch2);
	}

	/**
	 * 
	 * @param ch
	 * @param ch2
	 * @return
	 */
	public static LispCharSet getInterval(int ch, int ch2) {
		return new LispCharSet(getintv(ch, ch2));
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static LispCharSet parse(CharSequence s) {
		IntRange r = IntCharSets.parse(s);

		return (r != null) ? new LispCharSet(r) : null;
	}

	/**
	 * 
	 * @param c
	 * @return
	 */
	public boolean contains(int c) {
		return charset.contains(c);
	}

	/**
	 * 
	 * @return
	 */
	public LispCharSet complement() {
		return new LispCharSet(
				charset.difference(IntCharSets.ALL_CHAR));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isNeighborOf(net.morilib.lisp.Datum)
	 */
	public boolean isNeighborhoodOf(Datum d) {
		if(d instanceof LispCharacter) {
			return charset.contains(d.getCharacterCodePoint());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.AbstractLispTopology#isContained(net.morilib.lisp.topology.ILispTopology)
	 */
	@Override
	public boolean isContained(ILispTopology t) {
		if(t instanceof LispCharSet) {
			return charset.isContained(((LispCharSet)t).charset);
		} else {
			return super.isContained(t);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isIndependent(net.morilib.lisp.topology.ILispTopology)
	 */
	public boolean isIndependent(ILispTopology t) {
		if(t instanceof LispCharSet) {
			return charset.isIndependent(((LispCharSet)t).charset);
		} else {
			return super.isIndependent(t);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.AbstractLispTopology#unionTopology(net.morilib.lisp.topology.ILispTopology)
	 */
	@Override
	public ILispTopology unionTopology(ILispTopology t) {
		if(t instanceof LispCharSet) {
			return new LispCharSet(charset.join(
					((LispCharSet)t).charset));
		} else {
			return super.unionTopology(t);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.AbstractLispTopology#intersectTopology(net.morilib.lisp.topology.ILispTopology)
	 */
	@Override
	public ILispTopology intersectionTopology(ILispTopology t) {
		if(t instanceof LispCharSet) {
			return new LispCharSet(charset.meet(
					((LispCharSet)t).charset));
		} else {
			return super.intersectionTopology(t);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#interior()
	 */
	public ILispTopology interior() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#closure()
	 */
	public ILispTopology closure() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isOpen()
	 */
	public boolean isOpen() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isClosed()
	 */
	public boolean isClosed() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#cardinality()
	 */
	public LispCardinality cardinality() {
		return LispCardinality.finiteValueOf(charset.cardinality());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isEmpty()
	 */
	public boolean isEmpty() {
		return charset.cardinality() == 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispTopology#isUniverse()
	 */
	public boolean isUniverse() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispOrderedSet#maximum()
	 */
	public Datum maximum() {
		return LispCharacter.valueOf((char)charset.maximum());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispOrderedSet#minimum()
	 */
	public Datum minimum() {
		return LispCharacter.valueOf((char)charset.minimum());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispOrderedSet#supremum()
	 */
	public Datum supremum() {
		return LispCharacter.valueOf((char)charset.maximum());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispOrderedSet#infimum()
	 */
	public Datum infimum() {
		return LispCharacter.valueOf((char)charset.minimum());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#[");
		for(IntInterval i : charset.intervals()) {
			buf.appendCodePoint(i.minimum());
			if(i.minimum() != i.maximum()) {
				buf.append("-");
				buf.appendCodePoint(i.maximum());
			}
		}
		buf.append("]");
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return charset.hashCode();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof LispCharSet) {
			return charset.equals(((LispCharSet)o).charset);
		}
		return false;
	}

}
