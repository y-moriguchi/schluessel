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
package net.morilib.lisp.arith;

import net.morilib.lisp.LispReal;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/05
 */
public interface ILispRatio {

	/**
	 * @return the val1
	 */
	public LispReal getVal1();

	/**
	 * @return the val2
	 */
	public LispReal getVal2();

	/**
	 * 
	 * @return
	 */
	public LispReal getRatioValue();

	/**
	 * 
	 * @param r
	 * @return
	 */
	public LispReal toValue(LispReal r);

	/**
	 * 
	 * @param r
	 * @return
	 */
	public LispReal invertedValue(LispReal r);

}
