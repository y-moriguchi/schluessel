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
package net.morilib.parser.json;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/08/21
 */
public interface JSONParseHandler {

	/**
	 * 
	 */
	public void begin();

	/**
	 * @return 
	 * 
	 */
	public Object eof();

	/**
	 * 
	 */
	public void beginArray();

	/**
	 * 
	 */
	public void beginObject();

	/**
	 * 
	 */
	public void nextArray();

	/**
	 * 
	 */
	public void endArray();

	/**
	 * 
	 */
	public void nextValue();

	/**
	 * 
	 */
	public void nextObject();

	/**
	 * 
	 */
	public void endObject();

	/**
	 * @param string
	 */
	public void setString(String s);

	/**
	 * 
	 */
	public void setTrue();

	/**
	 * 
	 */
	public void setFalse();

	/**
	 * 
	 */
	public void setNull();

	/**
	 * @param getinteger
	 */
	public void setInteger(BigInteger x);

	/**
	 * @param getfraction
	 */
	public void setFraction(BigDecimal x);

	/**
	 * 
	 */
	public void emptyArray();

	/**
	 * 
	 */
	public void emptyObject();

}
