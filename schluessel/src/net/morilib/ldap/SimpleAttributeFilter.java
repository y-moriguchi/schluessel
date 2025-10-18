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
package net.morilib.ldap;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/14
 */
public class SimpleAttributeFilter implements AttributeFilter {

	//
	private AttributeType key;
	private String value;
	private FilterRelation rel;

	/**
	 * 
	 * @param key
	 * @param value
	 * @param rel
	 */
	public SimpleAttributeFilter(AttributeType key, String value,
			FilterRelation rel) {
		this.key   = key;
		this.value = value;
		this.rel   = rel;
	}

	/**
	 * 
	 * @param key
	 * @param value
	 * @param rel
	 */
	public SimpleAttributeFilter(String key, String value,
			FilterRelation rel) {
		this.key   = new AttributeType(key);
		this.value = value;
		this.rel   = rel;
	}

	/**
	 * 
	 * @return
	 */
	public AttributeType getKey() {
		return key;
	}

	/**
	 * 
	 * @return
	 */
	public String getValue() {
		return value;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return key.toString() + rel.toString() + value;
	}

}
