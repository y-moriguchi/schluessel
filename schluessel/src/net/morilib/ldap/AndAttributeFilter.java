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
public class AndAttributeFilter implements AttributeFilter {

	//
	private AttributeFilter[] filters;

	/**
	 * 
	 * @param filters
	 */
	public AndAttributeFilter(AttributeFilter... filters) {
		this.filters = new AttributeFilter[filters.length];
		System.arraycopy(filters, 0, this.filters, 0, filters.length);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuilder b = new StringBuilder();

		b.append("&");
		for(AttributeFilter f : filters) {
			b.append("(").append(f.toString()).append(")");
		}
		return b.toString();
	}

}
