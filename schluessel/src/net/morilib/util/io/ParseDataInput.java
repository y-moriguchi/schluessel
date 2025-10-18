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
package net.morilib.util.io;

import java.io.IOException;
import java.text.ParseException;

/**
 * This interface provides for reading Java primitive types
 * with parsing.
 * <p>Javaのプリミティブ型のデータをパースして読み込むことができることを
 * 示すインターフェースである.
 * 
 * @author MORIGUCHI, Yuichiro 2005/03/26
 */
public interface ParseDataInput {

	/**
	 * reads a boolean value from the input.
	 * <p>boolean値を読み込む.
	 * 
	 * @return     a boolean value
	 * @exception  ParseException if the input can not be parsed
	 * @exception  IOException    if an IO error occurs
	 */
	public boolean readBoolean() throws ParseException, IOException;

	/**
	 * reads a byte value from the input.
	 * <p>byte値を読み込む.
	 * 
	 * @return     a byte value
	 * @exception  ParseException if the input can not be parsed
	 * @exception  IOException    if an IO error occurs
	 */
	public byte readByte() throws ParseException, IOException;

	/**
	 * reads a short value from the input.
	 * <p>byte値を読み込む.
	 * 
	 * @return     a short value
	 * @exception  ParseException if the input can not be parsed
	 * @exception  IOException    if an IO error occurs
	 */
	public short readShort() throws ParseException, IOException;

	/**
	 * reads a char value from the input.
	 * <p>byte値を読み込む.
	 * 
	 * @return     a char value
	 * @exception  ParseException if the input can not be parsed
	 * @exception  IOException    if an IO error occurs
	 */
	public char readChar() throws ParseException, IOException;

	/**
	 * reads an int value from the input.
	 * <p>byte値を読み込む.
	 * 
	 * @return     an int value
	 * @exception  ParseException if the input can not be parsed
	 * @exception  IOException    if an IO error occurs
	 */
	public int readInt() throws ParseException, IOException;

	/**
	 * reads a long value from the input.
	 * <p>byte値を読み込む.
	 * 
	 * @return     a long value
	 * @exception  ParseException if the input can not be parsed
	 * @exception  IOException    if an IO error occurs
	 */
	public long readLong() throws ParseException, IOException;

	/**
	 * reads a float value from the input.
	 * <p>byte値を読み込む.
	 * 
	 * @return     a float value
	 * @exception  ParseException if the input can not be parsed
	 * @exception  IOException    if an IO error occurs
	 */
	public float readFloat() throws ParseException, IOException;

	/**
	 * reads a double value from the input.
	 * <p>byte値を読み込む.
	 * 
	 * @return     a double value
	 * @exception  ParseException if the input can not be parsed
	 * @exception  IOException    if an IO error occurs
	 */
	public double readDouble() throws ParseException, IOException;

	/**
	 * reads the next line of text from the input.
	 * <p>byte値を読み込む.
	 * 
	 * @return     a byte value
	 * @exception  ParseException if the input can not be parsed
	 * @exception  IOException    if an IO error occurs
	 */
	public String readLine() throws ParseException, IOException;

}
