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

import java.io.Closeable;
import java.io.IOException;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/01
 */
public interface OutputTranscoder extends Closeable {

	/**
	 * 
	 * @param c
	 */
	public void write(int c) throws IOException;

	/**
	 * 
	 * @param b
	 */
	public void write(String b) throws IOException;

	/**
	 * 
	 * @param b
	 * @param off
	 * @param len
	 */
	public void write(String b, int off, int len) throws IOException;

	/**
	 * 
	 * @param b
	 */
	public void write(int[] b) throws IOException;

	/**
	 * 
	 * @param b
	 * @param off
	 * @param len
	 */
	public void write(int[] b, int off, int len) throws IOException;

	/**
	 * 
	 * @throws IOException
	 */
	public void flush() throws IOException;

}
