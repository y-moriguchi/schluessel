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
public enum SyslogFacility {

	//
	KERNEL_MESSAGES(0),
	USER_LEVEL_MESSAGES(1),
	MAIL_SYSTEM(2),
	SYSTEM_DAEMONS(3),
	SECURITY_MESSAGES(4),
	MESSAGES_BY_SYSLOGD(5),
	LINE_PRINTER_SUBSYSTEM(6),
	NETWORK_NEWS_SUBSYSTEM(7),
	UUCP_SUBSYSTEM(8),
	CLOCK_DAEMON(9),
	SECURITY_MESSAGES2(10),
	FTP_DAEMON(11),
	NTP_SUBSYSTEM(12),
	LOG_AUDIT(13),
	LOG_ALERT(14),
	CLOCK_DAEMON2(15),
	LOCAL0(16),
	LOCAL1(17),
	LOCAL2(18),
	LOCAL3(19),
	LOCAL4(20),
	LOCAL5(21),
	LOCAL6(22),
	LOCAL7(23);

	//
	private int code;

	//
	private SyslogFacility(int code) {
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
