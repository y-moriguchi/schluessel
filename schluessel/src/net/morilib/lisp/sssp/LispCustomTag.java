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
package net.morilib.lisp.sssp;

import java.util.HashMap;
import java.util.Map;

import net.morilib.lisp.Datum2;
import net.morilib.lisp.Procedure;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/12/30
 */
public class LispCustomTag extends Datum2 {

	//
	/*package*/ Map<String, Integer> attrs;
	/*package*/ Map<String, TagTransformer> types;
	/*package*/ Procedure subr;

	/**
	 * 
	 * @param attrs
	 * @param types
	 * @param subr
	 */
	public LispCustomTag(Map<String, Integer> attrs,
			Map<String, TagTransformer> types,
			Procedure subr) {
		this.attrs = new HashMap<String, Integer>(attrs);
		this.types = new HashMap<String, TagTransformer>(types);
		this.subr  = subr;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<custom-tag>");
	}

}
