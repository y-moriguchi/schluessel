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
public interface ILispTextualOutputPort
extends ILispPort, ILispR6RSOutputPort {

	/**
	 * 
	 * @param c
	 */
	public void putChar(int c) throws IOException;

	/**
	 * 
	 * @param s
	 */
	public void putString(String s) throws IOException;

	/**
	 * 
	 * @param s
	 * @param start
	 */
	public void putString(String s, int start) throws IOException;

	/**
	 * 
	 * @param s
	 * @param start
	 * @param end
	 */
	public void putString(String s,
			int start, int end) throws IOException;

	/**
	 * 
	 * @param d
	 */
	public void putDatum(Datum d) throws IOException;

}
