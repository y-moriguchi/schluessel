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

import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Procedure;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/11
 */
public class LispRecordTypeDescriptor extends Datum2
implements RecordTypeDescriptor {

	//
	/*package*/ String id;
	/*package*/ RecordTypeDescriptor parent;
	/*package*/ String uid;
	/*package*/ boolean sealed;
	/*package*/ boolean opaque;
	/*package*/ Map<String, Boolean> fieldnames;
	private transient Set<String> allfieldnames = null;
	private transient Set<String> inheritedfieldnames = null;

	//
	private static
	ConcurrentHashMap<String, RecordTypeDescriptor> types =
		new ConcurrentHashMap<String, RecordTypeDescriptor>();
	private static
	ConcurrentHashMap<String, RecordTypeDescriptor> uids =
		new ConcurrentHashMap<String, RecordTypeDescriptor>();

	//
	/*package*/ LispRecordTypeDescriptor(
			String id, RecordTypeDescriptor parent,
			String uid, boolean sealed, boolean opaque,
			Map<String, Boolean> fieldnames) {
		this.id         = id;
		this.parent     = parent;
		this.uid        = uid;
		this.sealed     = sealed;
		this.opaque     = opaque;
		this.fieldnames = fieldnames;
	}

	/**
	 * 
	 * @param id
	 * @param parent
	 * @param uid
	 * @param sealed
	 * @param opaque
	 * @param fieldnames
	 * @return
	 * @throws RTDCreationException
	 */
	public static LispRecordTypeDescriptor newInstance(
			String id, RecordTypeDescriptor parent,
			String uid, boolean sealed, boolean opaque,
			Map<String, Boolean> fieldnames
			) throws RTDCreationException {
		if(id == null) {
			throw new NullPointerException();
		} else if(fieldnames == null) {
			throw new NullPointerException();
		} else if(uid != null && uids.containsKey(uid)) {
			throw new RTDCreationException();
		} else if(parent != null && parent.isSealed()) {
			throw new RTDCreationException();
		} else {
			LispRecordTypeDescriptor r;

			r = new LispRecordTypeDescriptor(
					id, parent, uid, sealed, opaque,
					new LinkedHashMap<String, Boolean>(fieldnames));
			types.put(id, r);
			if(uid != null) {
				uids.put(uid, r);
			}
			return r;
		}
	}

	/**
	 * 
	 * @param id
	 * @return
	 */
	public static RecordTypeDescriptor getInstance(String id) {
		return types.get(id);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.record.IRecordTypeDescriptor#getId()
	 */
	public String getId() {
		return id;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.record.IRecordTypeDescriptor#getParent()
	 */
	public RecordTypeDescriptor getParent() {
		return parent;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.record.IRecordTypeDescriptor#getUid()
	 */
	public String getUid() {
		return uid;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.record.IRecordTypeDescriptor#isSealed()
	 */
	public boolean isSealed() {
		return sealed;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.record.IRecordTypeDescriptor#isOpaque()
	 */
	public boolean isOpaque() {
		return opaque;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.record.IRecordTypeDescriptor#getFieldNames()
	 */
	public Set<String> getFieldNames() {
		return fieldnames.keySet();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.record.IRecordTypeDescriptor#getFieldNames()
	 */
	public Set<String> getAllFieldNames() {
		if(allfieldnames == null) {
			synchronized(this) {
				RecordTypeDescriptor rtd;

				allfieldnames = new HashSet<String>();
				for(rtd = this; rtd != null; rtd = rtd.getParent()) {
					allfieldnames.addAll(rtd.getFieldNames());
				}
			}
		}
		return allfieldnames;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.record.IRecordTypeDescriptor#getFieldNames()
	 */
	public Set<String> getInheritedFieldNames() {
		if(inheritedfieldnames == null) {
			synchronized(this) {
				RecordTypeDescriptor rtd;

				inheritedfieldnames = new HashSet<String>();
				for(rtd = parent; rtd != null; rtd = rtd.getParent()) {
					inheritedfieldnames.addAll(rtd.getFieldNames());
				}
			}
		}
		return inheritedfieldnames;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.record.IRecordTypeDescriptor#isMutable(java.lang.String)
	 */
	public boolean isMutable(String name) {
		RecordTypeDescriptor rtd;

		if(Boolean.TRUE.equals(fieldnames.get(name))) {
			return true;
		} else {
			for(rtd = parent; rtd != null; rtd = rtd.getParent()) {
				if(rtd.isMutable(name)) {
					return true;
				}
			}
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.record.IRecordTypeDescriptor#isMutable(java.lang.String)
	 */
	public boolean hasField(String name) {
		RecordTypeDescriptor rtd;

		if(fieldnames.containsKey(name)) {
			return true;
		} else {
			for(rtd = parent; rtd != null; rtd = rtd.getParent()) {
				if(rtd.hasField(name)) {
					return true;
				}
			}
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.record.RecordTypeDescriptor#getRcd()
	 */
	public RecordConstructorDescriptor getRcd(
			RecordTypeDescriptor rtd,
			RecordConstructorDescriptor parent,
			Procedure protocol) throws RCDCreationException {
		return new LispRecordConstructorDescriptor(
				rtd, parent, protocol);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.record.RecordTypeDescriptor#initialize(net.morilib.lisp.r6rs.record.R6RSRecord, net.morilib.lisp.Datum[])
	 */
	public void initialize(R6RSRecord rd,
			Datum... data) throws R6RSRecordInitilizeException {
		if(!(rd instanceof LispR6RSRecord)) {
			throw new R6RSRecordInitilizeException();
		} else if(!isInstance(rd)) {
			throw new R6RSRecordInitilizeException();
		} else if(rd.getRtd().getFieldNames().size() != data.length) {
			throw new R6RSRecordInitilizeException();
		} else {
			LispR6RSRecord rr = (LispR6RSRecord)rd;
			int i = 0;

			for(String s : fieldnames.keySet()) {
				rr.fields.put(s, data[i++]);
			}
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.record.RecordTypeDescriptor#isInstance(net.morilib.lisp.r6rs.record.R6RSRecord)
	 */
	public boolean isInstance(R6RSRecord r) {
		RecordTypeDescriptor rtd = r.getRtd();

		for(; rtd != null; rtd = rtd.getParent()) {
			if(equals(rtd)) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<record-type-descriptor ").append(id).append(">");
	}

}
