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
public class ExtensibleAttributeFilter implements AttributeFilter {

	/**
	 * 
	 */
	public static final boolean WITH_DN = true;

	/**
	 * 
	 */
	public static final boolean WITHOUT_DN = false;

	//
	private AttributeType key;
	private boolean dn;
	private String matchingRule, value;

	/**
	 * 
	 * @param key
	 * @param dn
	 * @param matchingRule
	 * @param value
	 */
	public ExtensibleAttributeFilter(AttributeType key,
			boolean dn, String matchingRule, String value) {
		if(key == null && matchingRule == null) {
			throw new NullPointerException();
		}
		this.key = key;
		this.dn  = dn;
		this.matchingRule = matchingRule;
		this.value = value;
	}

	/**
	 * 
	 * @param key
	 * @param dn
	 * @param matchingRule
	 * @param value
	 */
	public ExtensibleAttributeFilter(String key,
			boolean dn, String matchingRule, String value) {
		if(value == null) {
			throw new NullPointerException();
		} else if(key == null && matchingRule == null) {
			throw new NullPointerException();
		}
		this.key = new AttributeType(key);
		this.dn  = dn;
		this.matchingRule = matchingRule;
		this.value = value;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuilder b = new StringBuilder();

		if(key != null) {
			b.append(key.toString());
		}
		b.append(dn ? "" : ":dn");

		if(matchingRule != null) {
			b.append(":").append(matchingRule);
		}
		b.append(":=").append(value);
		return b.toString();
	}

}
