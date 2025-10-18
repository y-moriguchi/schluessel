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

import java.util.Collections;
import java.util.List;
import java.util.Map;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.r6rs.record.R6RSRecord;
import net.morilib.lisp.r6rs.record.RecordTypeDescriptor;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/03
 */
public class LispSimpleCondition extends Datum2
implements LispCondition, R6RSRecord {

	//
	private LispConditionType type;
	private Map<String, Datum> fields;

	/**
	 * 
	 * @param type
	 */
	public LispSimpleCondition(LispConditionType type) {
		this.type   = type;
		this.fields = type.makeFieldMap();
	}

	/**
	 * 
	 * @return
	 */
	public LispConditionType getConditionType() {
		return type;
	}

	/**
	 * 
	 * @param id
	 * @return
	 */
	public static LispSimpleCondition newInstance(String id) {
		LispConditionType t = LispConditionType.getInstance(id);

		return new LispSimpleCondition(
				(t == null) ? LispConditionType.CONDITION : t);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.condition.LispCondition#getField(java.lang.String)
	 */
	public Datum getField(String v) {
		return fields.get(v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.condition.LispCondition#setField(java.lang.String, net.morilib.lisp.LispMessage)
	 */
	public boolean setField(String v, Datum val) {
		if(!fields.containsKey(v)) {
			return false;
		}
		fields.put(v, val);
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.condition.LispCondition#hasType(net.morilib.lisp.condition.LispConditionType)
	 */
	public boolean hasType(LispConditionType type) {
		for(LispConditionType p = this.type; p != null; p = p.parent) {
			if(p.equals(type)) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.condition.LispCondition#getConditions()
	 */
	public List<LispSimpleCondition> getConditions() {
		return Collections.singletonList(this);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.condition.LispCondition#extractCondition(net.morilib.lisp.condition.LispConditionType)
	 */
	public LispSimpleCondition extractCondition(LispConditionType t) {
		return hasType(t) ? this : null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sos.ISlotDatum#getSlot(net.morilib.lisp.Symbol)
	 */
	public Datum getSlot(Symbol sym) {
		return getField(sym.getName());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sos.ISlotDatum#setSlot(net.morilib.lisp.Symbol, net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	public boolean setSlot(Symbol sym, Datum val, LispMessage mesg) {
		return setField(sym.getName(), val);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.record.R6RSRecord#isDefined(java.lang.String)
	 */
	public boolean isDefined(String v) {
		return fields.containsKey(v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.record.R6RSRecord#isMutable(java.lang.String)
	 */
	public boolean isMutable(String v) {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.record.R6RSRecord#getRtd()
	 */
	public RecordTypeDescriptor getRtd() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<condition ").append(type.getId()).append(">");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.condition.LispCondition#describeShort()
	 */
	public String describeShort() {
		return type.getId();
	}

}
