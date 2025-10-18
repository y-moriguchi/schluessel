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
package net.morilib.lisp.r6rs.io;

import java.io.IOException;

import net.morilib.lisp.Datum;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/01
 */
public interface ILispTextualInputPort
extends ILispPort, ILispR6RSInputPort {

	/**
	 * 
	 * @return
	 */
	public int getChar() throws IOException;

	/**
	 * 
	 * @return
	 */
	public int lookaheadChar() throws IOException;

	/**
	 * 
	 * @param n
	 * @return
	 */
	public String getString(int n) throws IOException;

	/**
	 * 
	 * @param buf
	 * @param start
	 * @param end
	 * @return
	 */
	public int getChars(int[] buf,
			int start, int end) throws IOException;

	/**
	 * 
	 * @return
	 */
	public String getStringAll() throws IOException;

	/**
	 * 
	 * @return
	 */
	public String getLine() throws IOException;

	/**
	 * 
	 * @return
	 */
	public Datum getDatum() throws IOException;

}
