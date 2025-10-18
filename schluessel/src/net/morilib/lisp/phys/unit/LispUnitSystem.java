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
package net.morilib.lisp.phys.unit;

import net.morilib.lisp.Datum2;
import net.morilib.phys.unit.SIUnitSystem;
import net.morilib.phys.unit.UnitSystem;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/05
 */
public class LispUnitSystem extends Datum2
implements java.io.Serializable {

	/**
	 * 
	 */
	public static final LispUnitSystem SI =
		new LispUnitSystem(SIUnitSystem.SI, "SI");

	//
	static LispUnitSystem defaultSystem = SI;
	static boolean unitVaild = true;

	//
	UnitSystem sys;
	private String desc;

	/**
	 * 
	 * @param sys
	 * @param desc
	 */
	public LispUnitSystem(UnitSystem sys, String desc) {
		this.sys  = sys;
		this.desc = desc;
	}

	/**
	 * @return
	 */
	public static UnitSystem getDefault() {
		return defaultSystem.sys;
	}

	/**
	 * 
	 * @return
	 */
	public static boolean isUnitValid() {
		return unitVaild;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<unit-system ").append(desc).append(">");
	}

}
