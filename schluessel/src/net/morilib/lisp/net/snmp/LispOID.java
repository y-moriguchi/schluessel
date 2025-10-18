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
package net.morilib.lisp.net.snmp;

import net.morilib.lisp.Datum2;
import net.morilib.net.snmp.ObjectIdentifier;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/15
 */
public class LispOID extends Datum2 {

	//
	/*package*/ ObjectIdentifier oid;

	/**
	 * 
	 * @param oid
	 */
	public LispOID(ObjectIdentifier oid) {
		if(oid == null) {
			throw new NullPointerException();
		}
		this.oid = oid;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof LispOID) {
			return oid.equals(((LispOID)o).oid);
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return oid.hashCode();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<object-identifier ").append(oid).append(">");
	}

}
