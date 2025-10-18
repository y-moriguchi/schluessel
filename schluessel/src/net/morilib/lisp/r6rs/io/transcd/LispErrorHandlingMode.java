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

import net.morilib.lisp.Datum2;
import net.morilib.util.io.TranscodeException;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/01
 */
public abstract class LispErrorHandlingMode extends Datum2 {

	/**
	 * 
	 */
	public static final int MODE_IGNORE = 0;

	/**
	 * 
	 */
	public static final int MODE_REPLACE = 1;

	/**
	 * 
	 */
	public static final int MODE_RAISE = 2;

	/**
	 * 
	 */
	public static final LispErrorHandlingMode
	IGNORE = new LispErrorHandlingMode(MODE_IGNORE) {

		@Override
		public int ifRead(int code, int replace) throws IOException {
			return -1;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.r6rs.io.LispErrorHandlingMode#ifWrite(int)
		 */
		@Override
		public int ifWrite(int code, int replace) throws IOException {
			return -1;
		}

	};

	/**
	 * 
	 */
	public static final LispErrorHandlingMode
	REPLACE = new LispErrorHandlingMode(MODE_REPLACE) {

		@Override
		public int ifRead(int code, int replace) throws IOException {
			return replace;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.r6rs.io.LispErrorHandlingMode#ifWrite(int)
		 */
		@Override
		public int ifWrite(int code, int replace) throws IOException {
			return replace;
		}

	};

	/**
	 * 
	 */
	public static final LispErrorHandlingMode
	RAISE = new LispErrorHandlingMode(MODE_RAISE) {

		@Override
		public int ifRead(int code, int replace) throws IOException {
			throw new TranscodeException(
					"Invalid code:" + Integer.toHexString(code));
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.r6rs.io.LispErrorHandlingMode#ifWrite(int)
		 */
		@Override
		public int ifWrite(int code, int replace) throws IOException {
			throw new TranscodeException(
					"Invalid code:" + Integer.toHexString(code));
		}

	};

	//
	private int mode;

	//
	/*package*/ LispErrorHandlingMode(int mode) {
		this.mode = mode;
	}

	/**
	 * 
	 * @return
	 */
	public int getMode() {
		return mode;
	}

	/**
	 * 
	 * @param code
	 * @return
	 * @throws IOException 
	 */
	public abstract int ifRead(int code,
			int replace) throws IOException;

	/**
	 * 
	 * @param code
	 * @return
	 * @throws IOException 
	 */
	public abstract int ifWrite(int code,
			int replace) throws IOException;

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<error-handling-mode ");
		switch(mode) {
		case MODE_IGNORE:   buf.append("ignore>");  break;
		case MODE_REPLACE:  buf.append("replace>");  break;
		case MODE_RAISE:    buf.append("raise>");  break;
		default:            buf.append("unknown>");  break;
		}
	}

}
