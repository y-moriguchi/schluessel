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
public enum FilterRelation {

	EQUAL("="), APPROX("~="), GREATER(">="), LESS("<=");

	//
	private String relop;

	//
	private FilterRelation(String relop) {
		this.relop = relop;
	}

	/* (non-Javadoc)
	 * @see java.lang.Enum#toString()
	 */
	public String toString() {
		return relop;
	}

}
