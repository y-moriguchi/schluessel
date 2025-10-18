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
package net.morilib.lisp.r6rs.enums;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Procedure;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/21
 */
public interface ILispR6RSEnum {

	/**
	 * 
	 * @return
	 */
	public Datum getUniverse();

	/**
	 * 
	 * @return
	 */
	public Procedure getIndexer();

	/**
	 * 
	 * @return
	 */
	public Procedure getConstructor();

	/**
	 * 
	 * @return
	 */
	public Datum toList();

	/**
	 * 
	 * @param d
	 * @return
	 */
	public boolean contains(Datum d);

	/**
	 * 
	 * @return
	 */
	public ILispR6RSEnum getUniverseEnumSet();

	/**
	 * 
	 * @param s
	 * @return
	 */
	public boolean isIn(ILispR6RSEnum s);

	/**
	 * 
	 * @param s
	 * @return
	 */
	public boolean isEqualTo(ILispR6RSEnum s);

	/**
	 * 
	 * @return
	 */
	public ILispR6RSEnum complement();

}
