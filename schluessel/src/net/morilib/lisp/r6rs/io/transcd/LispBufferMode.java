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

import java.io.BufferedOutputStream;
import java.io.OutputStream;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Symbol;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/01
 */
public abstract class LispBufferMode extends Datum2 {

	/**
	 * 
	 */
	public static final LispBufferMode
	NONE = new LispBufferMode("none") {

		@Override
		public OutputTranscoder getTranscoder(OutputTranscoder tr) {
			return tr;
		}

		@Override
		public OutputStream getOutputStream(OutputStream ous) {
			return ous;
		}

	};

	/**
	 * 
	 */
	public static final LispBufferMode
	LINE = new LispBufferMode("line") {

		@Override
		public OutputTranscoder getTranscoder(OutputTranscoder tr) {
			return new LineBufferedOutputTranscoder(tr);
		}

		@Override
		public OutputStream getOutputStream(OutputStream ous) {
			return new BufferedOutputStream(ous);
		}

	};

	/**
	 * 
	 */
	public static final LispBufferMode
	BLOCK = new LispBufferMode("block") {

		@Override
		public OutputTranscoder getTranscoder(OutputTranscoder tr) {
			return new BlockBufferedOutputTranscoder(tr);
		}

		@Override
		public OutputStream getOutputStream(OutputStream ous) {
			return new BufferedOutputStream(ous);
		}

	};

	//
	private static final Symbol SYM_NONE  = Symbol.getSymbol("none");
	private static final Symbol SYM_LINE  = Symbol.getSymbol("line");
	private static final Symbol SYM_BLOCK = Symbol.getSymbol("block");

	//
	private String symname;

	//
	private LispBufferMode(String symname) {
		this.symname = symname;
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static LispBufferMode getInstance(Datum d) {
		if(d.equals(SYM_NONE)) {
			return NONE;
		} else if(d.equals(SYM_LINE)) {
			return LINE;
		} else if(d.equals(SYM_BLOCK)) {
			return BLOCK;
		} else {
			return null;
		}
	}

	/**
	 * 
	 * @param tr
	 * @return
	 */
	public abstract OutputTranscoder getTranscoder(
			OutputTranscoder tr);

	/**
	 * 
	 * @param tr
	 * @return
	 */
	public abstract OutputStream getOutputStream(
			OutputStream ous);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<file-option ").append(symname).append(">");
	}

}
