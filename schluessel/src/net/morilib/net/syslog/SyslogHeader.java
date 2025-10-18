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

import java.text.SimpleDateFormat;
import java.util.Locale;
import java.util.regex.Pattern;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/18
 */
public class SyslogHeader {

	//
	private static final Pattern PTN1 = Pattern.compile(
			"[a-zA-Z]([\\-a-zA-Z0-9]*[a-zA-Z0-9])?");
	private static final Pattern PTN2 = Pattern.compile(
			"([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\\." +
			"([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\\." +
			"([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\\." +
			"([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])");
	private static final Pattern PTN3 = Pattern.compile(
			"(([0-9A-Fa-f]{1,4}:){7}[0-9A-Fa-f]{1,4})|" +
			"(([0-9A-Fa-f]{1,4}:){6}::[0-9A-Fa-f]{1,4})|" +
			"(([0-9A-Fa-f]{1,4}:){5}::" +
			"([0-9A-Fa-f]{1,4}:){0,1}[0-9A-Fa-f]{1,4})|" +
			"(([0-9A-Fa-f]{1,4}:){4}::" +
			"([0-9A-Fa-f]{1,4}:){0,2}[0-9A-Fa-f]{1,4})|" +
			"(([0-9A-Fa-f]{1,4}:){3}::" +
			"([0-9A-Fa-f]{1,4}:){0,3}[0-9A-Fa-f]{1,4})|" +
			"(([0-9A-Fa-f]{1,4}:){2}::" +
			"([0-9A-Fa-f]{1,4}:){0,4}[0-9A-Fa-f]{1,4})|" +
			"(([0-9A-Fa-f]{1,4}:){1}::" +
			"([0-9A-Fa-f]{1,4}:){0,5}[0-9A-Fa-f]{1,4})|" +
			"(::([0-9A-Fa-f]{1,4}:){0,6}[0-9A-Fa-f]{1,4})|" +
			"(([0-9A-Fa-f]{1,4}:){0,7}::)|" +
			"(::([fF]{4}:)?" +
			"([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\\." +
			"([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\\." +
			"([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])\\." +
			"([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5]))");
	private static final SimpleDateFormat FMT1 =
		new SimpleDateFormat("MMM dd HH:mm:ss", Locale.US);

	//
	private java.util.Date timestamp;
	private String hostname;

	/**
	 * 
	 * @param timestamp
	 * @param hostname
	 */
	public SyslogHeader(java.util.Date timestamp, String hostname) {
		if(timestamp == null || hostname == null) {
			throw new NullPointerException();
		}
		this.timestamp = timestamp;
		this.hostname  = hostname;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isValidHostname() {
		return (PTN1.matcher(hostname).matches() ||
				PTN2.matcher(hostname).matches() ||
				PTN3.matcher(hostname).matches());
	}

	/**
	 * 
	 * @return
	 */
	public java.util.Date getTimestamp() {
		return timestamp;
	}

	/**
	 * 
	 * @return
	 */
	public String getHostname() {
		return hostname;
	}

	/**
	 * 
	 * @return
	 */
	public String toSyslog() {
		StringBuilder d = new StringBuilder(FMT1.format(timestamp));

		if(d.charAt(4) == '0') {
			d.setCharAt(4, ' ');
		}
		d.append(' ').append(hostname);
		return d.toString();
	}

}
