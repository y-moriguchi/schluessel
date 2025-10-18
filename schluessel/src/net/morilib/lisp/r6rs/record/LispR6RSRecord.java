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
package net.morilib.lisp.r6rs.record;

import java.util.LinkedHashMap;
import java.util.Map;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.Undef;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/13
 */
public class LispR6RSRecord extends Datum2 implements R6RSRecord {

	//
	private RecordTypeDescriptor rtd;
	/*package*/ Map<String, Datum> fields;

	/**
	 * 
	 * @param rtd
	 */
	public LispR6RSRecord(RecordTypeDescriptor rtd) {
		RecordTypeDescriptor r;

		this.rtd = rtd;
		this.fields = new LinkedHashMap<String, Datum>();
		for(r = rtd; r != null; r = r.getParent()) {
			for(String s : r.getFieldNames()) {
				fields.put(s, Undef.UNDEF);
			}
		}
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
	 * @see net.morilib.lisp.r6rs.record.R6RSRecord#getField(java.lang.String)
	 */
	public Datum getField(String v) {
		return fields.get(v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.record.R6RSRecord#setField(java.lang.String, net.morilib.lisp.Datum)
	 */
	public boolean setField(String v, Datum val) {
		if(!fields.containsKey(v)) {
			return false;
		} else if(!rtd.isMutable(v)) {
			return false;
		} else {
			fields.put(v, val);
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.record.R6RSRecord#isMutable(java.lang.String)
	 */
	public boolean isMutable(String v) {
		return rtd.isMutable(v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.record.R6RSRecord#isDefined(java.lang.String)
	 */
	public boolean isDefined(String v) {
		return fields.containsKey(v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.record.R6RSRecord#getRtd()
	 */
	public RecordTypeDescriptor getRtd() {
		return rtd;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<r6rs-record ").append(rtd.getId()).append(">");
	}

}
