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
public class AttributeType {

	//
	private String name;
	private String[] options;

	/**
	 * 
	 * @param name
	 */
	public AttributeType(String name) {
		this.name = name;
		this.options = new String[0];
	}

	/**
	 * 
	 * @param name
	 * @param opts
	 */
	public AttributeType(String name, String[] opts) {
		this.name = name;
		this.options = new String[opts.length];
		System.arraycopy(opts, 0, this.options, 0, opts.length);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuilder b = new StringBuilder(name);

		for(String o : options) {
			b.append(";").append(o);
		}
		return b.toString();
	}

}
