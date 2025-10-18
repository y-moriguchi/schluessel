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

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import net.morilib.lisp.math.algebra.ILispAddable;
import net.morilib.lisp.math.algebra.ILispMultipliable;
import net.morilib.lisp.sos.LispType;
import net.morilib.lisp.subr.UnaryArgs;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class RegexPattern extends Datum
implements ILispAddable<RegexPattern>,
ILispMultipliable<RegexPattern> {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/04/09
	 */
	public static class Match extends Datum {

		//
		private Matcher match;
		//private int ptr = 0;

		//
		private Match(Matcher x) {
			match = x;
		}

		/**
		 * 
		 * @return
		 */
		public Datum group() {
			return new LispString(match.group());
		}

		/**
		 * 
		 * @param no
		 * @return
		 */
		public Datum group(int no) {
			String res = match.group(no);

			return (res != null) ? new LispString(res) : null;
		}

		//
		/*private boolean hasNext() {
			return ptr <= match.groupCount();
		}

		//
		private Datum next() {
			return group(ptr++);
		}

		//
		private void remove() {
			throw new UnsupportedOperationException();
		}*/

		/**
		 * 
		 */
		public String toStringRepl() {
			return match.toString();
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
		 */
		@Override
		public void toDisplayString(StringBuilder buf) {
			buf.append("#<rxmatch " + toStringRepl() + ">");
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/04/09
	 */
	public final static class CompileRe extends UnaryArgs {

		/*
		 * (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispString) {
				String  tok = ((LispString)c1a).getString();
				Matcher mch = REGEX_PAT.matcher(tok);

				if(!mch.matches()) {
					throw mesg.getError("err.regex.syntax", tok);
				}
				//mch.find();

				String  re  = mch.group(1);
				String  fl  = mch.group(2);

				try {
					return new RegexPattern(re, fl);
				} catch(PatternSyntaxException e) {
					throw mesg.getError("err.regex.syntax", tok);
				}
			} else {
				throw mesg.getError("err.require.string", c1a);
			}
		}

	}

	//
	private static final Pattern REGEX_PAT =
		Pattern.compile("/(.*)/([iuc]*)");

	//
	private Pattern pattern;
	private String patternString;
	private String patternString2;
	private String flags;

	/**
	 * 
	 * @param regex
	 * @param fl
	 */
	public RegexPattern(String regex, String fl) {
		int flg = 0;

		for(int i = 0; i < fl.length(); i++) {
			switch(fl.charAt(i)) {
			case 'u':
				flg |= Pattern.UNICODE_CASE;
				break;
			case 'i':
				flg |= Pattern.CASE_INSENSITIVE;
				break;
			case 'c':
				flg |= Pattern.CANON_EQ;
				break;
			default:
				throw new IllegalArgumentException(fl);
			}
		}
		pattern = Pattern.compile(regex, flg);
		patternString  = "#/" + regex + "/" + fl;
		patternString2 = regex;
		flags = fl;
	}

	/**
	 * 
	 * @param str
	 * @return
	 */
	public Datum getMatch(String str) {
		Matcher mt = pattern.matcher(str);

		return mt.matches() ? new Match(mt) : LispBoolean.FALSE;
	}

	/**
	 * @return the pattern
	 */
	public Pattern getPattern() {
		return pattern;
	}

	/**
	 * 
	 * @return
	 */
	public String getPatternString() {
		return patternString;
	}

	/**
	 * 
	 * @return
	 */
	public String getPatternString2() {
		return patternString2;
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public boolean matches(CharSequence s) {
		return pattern.matcher(s).matches();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispMultipliable#mul(net.morilib.lisp.math.algebra.ILispMultipliable)
	 */
	public RegexPattern mul(RegexPattern y) {
		return new RegexPattern(
				"(" + patternString2 + y.patternString2 + ")",
				flags);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispAddable#add(net.morilib.lisp.math.algebra.ILispAddable)
	 */
	public RegexPattern add(RegexPattern y) {
		return new RegexPattern(
				"(" + patternString2 + "|" + y.patternString2 + ")",
				flags);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getType()
	 */
	@Override
	public LispType getType() {
		return LispType.REGEXP;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<regexp " + getPatternString() + ">");
	}

}
