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
package net.morilib.lisp.condition;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/03
 */
public class LispConditionType extends Datum2 {

	/**
	 * 
	 */
	public static final LispConditionType CONDITION;

	//
	private static ConcurrentHashMap<String, LispConditionType> types;

	//
	static {
		CONDITION = new LispConditionType("&condition", null);
		types = new ConcurrentHashMap<String, LispConditionType>();
		types.putIfAbsent("&condition", CONDITION);
	}

	//
	private     String id;
	/*package*/ LispConditionType parent;
	private     Set<String> fieldnames;

	//
	private LispConditionType(String id,
			LispConditionType parent, Collection<String> fields) {
		this.id         = id;
		this.parent     = parent;
		this.fieldnames = new HashSet<String>(fields);
	}

	/**
	 * 
	 * @param id
	 * @param parent
	 * @param fields
	 * @return
	 */
	public static LispConditionType newInstance(String id,
			LispConditionType parent, Collection<String> fields) {
		LispConditionType r;

		r = new LispConditionType(id, parent, fields);
		types.put(id, r);
		return r;
	}

	/**
	 * 
	 * @param id
	 * @return
	 */
	public static LispConditionType getInstance(String id) {
		return types.get(id);
	}

	/**
	 * 
	 * @param id
	 * @param parent
	 * @param fields
	 */
	public LispConditionType(String id,
			LispConditionType parent, String... fields) {
		this(id, parent, Arrays.asList(fields));
	}

	/**
	 * @return
	 */
	public String getId() {
		return id;
	}

	/**
	 * 
	 * @return
	 */
	public LispConditionType getParent() {
		return parent;
	}

	/**
	 * 
	 * @return
	 */
	public Set<String> getFieldNames() {
		return Collections.unmodifiableSet(fieldnames);
	}

	//
	/*package*/ Map<String, Datum> makeFieldMap() {
		Map<String, Datum> r = new HashMap<String, Datum>();
		LispConditionType  p = this;

		for(; p != null; p = p.parent) {
			for(String s : p.fieldnames) {
				r.put(s, null);
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<condition-type ").append(id).append(">");
	}

}
