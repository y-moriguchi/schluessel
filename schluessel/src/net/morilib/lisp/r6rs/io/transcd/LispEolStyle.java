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
package net.morilib.lisp.r6rs.io.transcd;

import java.util.Arrays;

import net.morilib.lisp.Datum2;
import net.morilib.util.io.UTF16;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/01
 */
public class LispEolStyle extends Datum2 {

	/**
	 * 
	 */
	public static final LispEolStyle CR = new LispEolStyle(
			new int[] { '\r' });

	/**
	 * 
	 */
	public static final LispEolStyle LF = new LispEolStyle(
			new int[] { '\n' });

	/**
	 * 
	 */
	public static final LispEolStyle CRLF = new LispEolStyle(
			new int[] { '\r', '\n' });

	/**
	 * 
	 */
	public static final LispEolStyle NEL = new LispEolStyle(
			new int[] { '\u0085' });

	/**
	 * 
	 */
	public static final LispEolStyle CRNEL = new LispEolStyle(
			new int[] { '\r', '\u0085' });

	/**
	 * 
	 */
	public static final LispEolStyle LS = new LispEolStyle(
			new int[] { '\u2028' });

	/**
	 * 
	 */
	public static final LispEolStyle NONE = new LispEolStyle(null) {

		@Override
		public InputTranscoder getInputTranscoder(InputTranscoder tr) {
			return tr;
		}

		@Override
		public OutputTranscoder getOutputTranscoder(
				OutputTranscoder tr) {
			return tr;
		}

	};

	//
	/*package*/ int[] newline;

	//
	/*package*/ LispEolStyle(int[] newline) {
		this.newline = newline;
	}

	/**
	 * 
	 * @param newline
	 * @return
	 */
	public static LispEolStyle getInstance(int[] newline) {
		int[] l = new int[newline.length];

		System.arraycopy(newline, 0, l, 0, l.length);
		return new LispEolStyle(l);
	}

	/**
	 * 
	 * @return
	 */
	public static LispEolStyle getNative() {
		String s = System.getProperty("line.separator");

		if(s == null || s.length() == 0) {
			throw new RuntimeException();
		}
		return new LispEolStyle(UTF16.getInts(s));
	}

	/**
	 * 
	 * @param tr
	 * @return
	 */
	public InputTranscoder getInputTranscoder(InputTranscoder tr) {
		return new NewlineInputTranscoder(tr, this, '\n');
	}

	/**
	 * 
	 * @param tr
	 * @return
	 */
	public OutputTranscoder getOutputTranscoder(OutputTranscoder tr) {
		return new NewlineOutputTranscoder(tr, this);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return Arrays.hashCode(newline);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof LispEolStyle) {
			return Arrays.equals(newline, ((LispEolStyle)o).newline);
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<eol-style ");
		if(newline == null) {
			buf.append("none");
		} else if(Arrays.equals(newline, CR.newline)) {
			buf.append("cr");
		} else if(Arrays.equals(newline, LF.newline)) {
			buf.append("lf");
		} else if(Arrays.equals(newline, CRLF.newline)) {
			buf.append("crlf");
		} else if(Arrays.equals(newline, NEL.newline)) {
			buf.append("nel");
		} else if(Arrays.equals(newline, CRNEL.newline)) {
			buf.append("crnel");
		} else if(Arrays.equals(newline, LS.newline)) {
			buf.append("ls");
		} else {
			buf.append("unknown");
		}
		buf.append(">");
	}

}
