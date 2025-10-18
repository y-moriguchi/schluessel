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
package net.morilib.net.snmp;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import net.morilib.util.Tuple2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/15
 */
public class SNMPPDU {

	//
	private int version;
	private String community;
//	private SNMPPDUType type;
	private int id;
	private int errorCode;
	private int errorIndex;
	private List<Tuple2<ObjectIdentifier, Object>> variables;

	/**
	 * @return the version
	 */
	public int getVersion() {
		return version;
	}

	/**
	 * @param version the version to set
	 */
	public void setVersion(int version) {
		this.version = version;
	}

	/**
	 * @return the community
	 */
	public String getCommunity() {
		return community;
	}

	/**
	 * @param community the community to set
	 */
	public void setCommunity(String community) {
		this.community = community;
	}

//	/**
//	 * @return the type
//	 */
//	public SNMPPDUType getType() {
//		return type;
//	}
//
//	/**
//	 * @param type the type to set
//	 */
//	public void setType(SNMPPDUType type) {
//		if((this.type = type) == null) {
//			throw new NullPointerException();
//		}
//	}

	/**
	 * @return the id
	 */
	public int getId() {
		return id;
	}

	/**
	 * @param id the id to set
	 */
	public void setId(int id) {
		this.id = id;
	}

	/**
	 * @return the errorCode
	 */
	public int getErrorCode() {
		return errorCode;
	}

	/**
	 * @param errorCode the errorCode to set
	 */
	public void setErrorCode(int errorCode) {
		this.errorCode = errorCode;
	}

	/**
	 * @return the errorIndex
	 */
	public int getErrorIndex() {
		return errorIndex;
	}

	/**
	 * @param errorIndex the errorIndex to set
	 */
	public void setErrorIndex(int errorIndex) {
		this.errorIndex = errorIndex;
	}

	/**
	 * @return the variables
	 */
	public List<Tuple2<ObjectIdentifier, Object>> getVariables() {
		return Collections.unmodifiableList(variables);
	}

	/**
	 * @param variables the variables to set
	 */
	public void setVariables(
			List<Tuple2<ObjectIdentifier, Object>> variables) {
		this.variables =
			new ArrayList<Tuple2<ObjectIdentifier, Object>>(variables);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuilder b = new StringBuilder();

		b.append("version=").append(version);
		b.append(", community=").append(community);
//		b.append(", type=").append(type);
		b.append(", id=").append(id);
		b.append(", error code=").append(errorCode);
		b.append(", error index=").append(errorIndex);
		if(variables != null) {
			b.append(", variable=").append(variables);
		}
		return b.toString();
	}

}
