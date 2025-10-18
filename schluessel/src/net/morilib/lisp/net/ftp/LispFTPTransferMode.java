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
package net.morilib.lisp.net.ftp;

import net.morilib.lisp.Datum2;
import net.morilib.net.ftp.FTPMode;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/14
 */
public class LispFTPTransferMode extends Datum2 {

	/**
	 * 
	 */
	public static final LispFTPTransferMode BINARY =
		new LispFTPTransferMode(FTPMode.BINARY);

	/**
	 * 
	 */
	public static final LispFTPTransferMode ASCII =
		new LispFTPTransferMode(FTPMode.ASCII);

	//
	/*package*/ FTPMode mode;

	//
	private LispFTPTransferMode(FTPMode mode) {
		this.mode = mode;
	}

	/**
	 * 
	 * @param mode
	 */
	public static LispFTPTransferMode getInstance(FTPMode mode) {
		switch(mode) {
		case BINARY:  return BINARY;
		case ASCII:   return ASCII;
		default:
			throw new IllegalArgumentException();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<ftp-transfer-mode ").append(mode.toString())
		.append(">");
	}

}
