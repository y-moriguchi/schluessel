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

import java.io.InputStream;
import java.io.OutputStream;

import net.morilib.lisp.Datum2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/01
 */
public class LispUTF8Codec extends Datum2 implements ILispCodec {

	/**
	 * 
	 */
	public static final LispUTF8Codec INSTANCE = new LispUTF8Codec();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.TranscoderFactory#newInput(java.io.InputStream, java.lang.String)
	 */
	public InputTranscoder newInput(InputStream ins, LispEolStyle eol,
			LispErrorHandlingMode mode) {
		return eol.getInputTranscoder(
				new UTF8InputTranscoder(ins, mode));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.TranscoderFactory#newOutput(java.io.OutputStream, java.lang.String)
	 */
	public OutputTranscoder newOutput(OutputStream ous,
			LispEolStyle eol, LispErrorHandlingMode mode) {
		return eol.getOutputTranscoder(
				new UTF8OutputTranscoder(ous, mode));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<latin-1-codec>");
	}

}
