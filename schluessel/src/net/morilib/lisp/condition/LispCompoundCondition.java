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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Symbol;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/03
 */
public class LispCompoundCondition extends Datum2
implements LispCondition {

	//
	private List<LispSimpleCondition> conditions;

	/**
	 * 
	 * @param cs
	 */
	public LispCompoundCondition(Collection<LispCondition> cs) {
		conditions = new ArrayList<LispSimpleCondition>();

		for(LispCondition c : cs) {
			conditions.addAll(c.getConditions());
		}
	}

	/**
	 * 
	 * @param cs
	 */
	public LispCompoundCondition(LispCondition... cs) {
		this(Arrays.asList(cs));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.condition.LispCondition#getField(java.lang.String)
	 */
	public Datum getField(String v) {
		Datum d;

		for(LispSimpleCondition c : conditions) {
			if((d = c.getField(v)) != null) {
				return d;
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.condition.LispCondition#setField(java.lang.String, net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	public boolean setField(String v, Datum val) {
		for(LispSimpleCondition c : conditions) {
			if(c.setField(v, val)) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.condition.LispCondition#hasType(net.morilib.lisp.condition.LispConditionType)
	 */
	public boolean hasType(LispConditionType type) {
		for(LispSimpleCondition c : conditions) {
			if(c.hasType(type)) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.condition.LispCondition#getConditions()
	 */
	public List<LispSimpleCondition> getConditions() {
		return Collections.unmodifiableList(conditions);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.condition.LispCondition#extractCondition(net.morilib.lisp.condition.LispConditionType)
	 */
	public LispSimpleCondition extractCondition(LispConditionType t) {
		LispSimpleCondition d;

		for(LispSimpleCondition c : conditions) {
			if((d = c.extractCondition(t)) != null) {
				return d;
			}
		}
		return null;
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
	 * @see net.morilib.lisp.r6rs.record.R6RSRecord#isMutable(java.lang.String)
	 */
	public boolean isDefined(String v) {
		for(LispSimpleCondition c : conditions) {
			if(c.isDefined(v)) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.record.R6RSRecord#isMutable(java.lang.String)
	 */
	public boolean isMutable(String v) {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<condition");
		for(LispSimpleCondition c : conditions) {
			buf.append(" ").append(c.getConditionType().getId());
		}
		buf.append(">");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.condition.LispCondition#describeShort()
	 */
	public String describeShort() {
		StringBuilder b = new StringBuilder();
		String dlm = "";

		for(LispSimpleCondition c : conditions) {
			b.append(dlm);
			b.append(c.getConditionType().getId());
			dlm = ", ";
		}
		return b.toString();
	}

}
