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
package net.morilib.net.syslog;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/18
 */
public enum SyslogLevel {

	//
	EMERGENCY(0),
	ALERT(1),
	CRITICAL(2),
	ERROR(3),
	WARNING(4),
	NOTICE(5),
	INFORMATIONAL(6),
	DEBUG(7);

	//
	private int code;

	//
	private SyslogLevel(int code) {
		this.code = code;
	}

	/**
	 * 
	 * @return
	 */
	public int getCode() {
		return code;
	}

}
