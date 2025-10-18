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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import net.morilib.lisp.Datum2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/02
 */
public class LispTranscoder extends Datum2 implements ILispTranscoder {

	/**
	 * 
	 */
	public static final LispTranscoder STRING =
		new LispTranscoder(LispUTF16Codec.INSTANCE,
				LispEolStyle.LF, LispErrorHandlingMode.REPLACE);

	//
	private ILispCodec codec;
	private LispEolStyle eol;
	private LispErrorHandlingMode mode;

	/**
	 * 
	 * @param codec
	 * @param eol
	 * @param mode
	 */
	public LispTranscoder(ILispCodec codec, LispEolStyle eol,
			LispErrorHandlingMode mode) {
		this.codec = codec;
		this.eol   = eol;
		this.mode  = mode;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.ILispTranscoder#newInput(java.io.InputStream)
	 */
	public InputTranscoder newInput(
			InputStream ins) throws IOException {
		return codec.newInput(ins, eol, mode);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.ILispTranscoder#newOutput(java.io.OutputStream)
	 */
	public OutputTranscoder newOutput(
			OutputStream ous) throws IOException {
		return codec.newOutput(ous, eol, mode);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.ILispTranscoder#getCodec()
	 */
	public ILispCodec getCodec() {
		return codec;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.ILispTranscoder#getEol()
	 */
	public LispEolStyle getEol() {
		return eol;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.transcd.ILispTranscoder#getMode()
	 */
	public LispErrorHandlingMode getMode() {
		return mode;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<transcoder>");
	}

}
