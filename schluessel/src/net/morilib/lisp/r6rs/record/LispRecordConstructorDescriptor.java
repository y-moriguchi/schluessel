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

import net.morilib.lisp.Datum2;
import net.morilib.lisp.Procedure;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/11
 */
public class LispRecordConstructorDescriptor extends Datum2
implements RecordConstructorDescriptor {

	//
	private RecordTypeDescriptor rtd;
	private RecordConstructorDescriptor parent;
	private Procedure protocol;

	/**
	 * 
	 * @param rtd
	 * @param parent
	 * @param protocol
	 * @throws RCDCreationException
	 */
	public LispRecordConstructorDescriptor(
			RecordTypeDescriptor rtd,
			RecordConstructorDescriptor parent,
			Procedure protocol) throws RCDCreationException {
		if(rtd == null) {
			throw new NullPointerException();
		} else if(rtd.getParent() == null && parent != null) {
			throw new RCDCreationException();
		}
		this.rtd      = rtd;
		this.parent   = parent;
		this.protocol = (protocol == null) ?
				new DefaultProtocol(rtd) : protocol;
	}

	/**
	 * @return the rtd
	 */
	public RecordTypeDescriptor getRtd() {
		return rtd;
	}

	/**
	 * @return the parent
	 */
	public RecordConstructorDescriptor getParent() {
		return parent;
	}

	/**
	 * @return the protocol
	 */
	public Procedure getProtocol() {
		return protocol;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.record.RecordConstructorDescriptor#newInstance()
	 */
	public R6RSRecord newInstance() {
		return new LispR6RSRecord(rtd);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<record-constructor-descriptor ")
		.append(rtd.getId())
		.append(">");
	}

}
