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

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/18
 */
public final class SyslogP {

	//
	private static final int PORT = 514;

	//
	private SyslogP() {}

	/**
	 * 
	 * @param addr
	 * @param port
	 * @param level
	 * @param facility
	 * @param hostname
	 * @param message
	 * @return
	 * @throws IOException
	 */
	public static boolean log(InetAddress addr, int port,
			SyslogLevel level, SyslogFacility facility,
			java.util.Date date, String hostname,
			String message) throws IOException {
		DatagramSocket sok = new DatagramSocket();
		DatagramPacket pkt;
		StringBuilder s = new StringBuilder();
		byte[] b;
		SyslogPRI pri;
		SyslogHeader hdr;

		pri = new SyslogPRI(facility, level);
		s.append(pri.toSyslogPriority());
		hdr = new SyslogHeader(date, hostname);

		if(!hdr.isValidHostname()) {
			return false;
		}
		s.append(hdr.toSyslog());
		s.append(' ');
		s.append(message);

		for(int i = 0; i < s.length(); i++) {
			if(s.charAt(i) > 127) {
				return false;
			}
		}
		b   = s.toString().getBytes();
		pkt = new DatagramPacket(b, b.length, addr, port);
		sok.send(pkt);
		return true;
	}

	/**
	 * 
	 * @param addr
	 * @param port
	 * @param level
	 * @param facility
	 * @param hostname
	 * @param message
	 * @return
	 * @throws IOException
	 */
	public static boolean log(InetAddress addr, int port,
			SyslogLevel level, SyslogFacility facility,
			String hostname, String message) throws IOException {
		return log(addr, port, level, facility,
				new java.util.Date(), hostname, message);
	}

	/**
	 * 
	 * @param addr
	 * @param level
	 * @param facility
	 * @param date
	 * @param hostname
	 * @param message
	 * @return
	 * @throws IOException
	 */
	public static boolean log(InetAddress addr,
			SyslogLevel level, SyslogFacility facility,
			java.util.Date date, String hostname,
			String message) throws IOException {
		return log(addr, PORT, level, facility, date, hostname,
				message);
	}

	/**
	 * 
	 * @param addr
	 * @param level
	 * @param facility
	 * @param hostname
	 * @param message
	 * @return
	 * @throws IOException
	 */
	public static boolean log(InetAddress addr,
			SyslogLevel level, SyslogFacility facility,
			String hostname, String message) throws IOException {
		return log(addr, PORT, level, facility,
				new java.util.Date(), hostname, message);
	}

	/**
	 * 
	 * @param host
	 * @param port
	 * @param level
	 * @param facility
	 * @param date
	 * @param hostname
	 * @param message
	 * @return
	 * @throws IOException
	 */
	public static boolean log(String host, int port,
			SyslogLevel level, SyslogFacility facility,
			java.util.Date date, String hostname,
			String message) throws IOException {
		return log(InetAddress.getByName(host), port, level,
				facility, date, hostname, message);
	}

	/**
	 * 
	 * @param host
	 * @param port
	 * @param level
	 * @param facility
	 * @param hostname
	 * @param message
	 * @return
	 * @throws IOException
	 */
	public static boolean log(String host, int port,
			SyslogLevel level, SyslogFacility facility,
			String hostname, String message) throws IOException {
		return log(InetAddress.getByName(host), port, level,
				facility, new java.util.Date(), hostname, message);
	}

	/**
	 * 
	 * @param host
	 * @param level
	 * @param facility
	 * @param date
	 * @param hostname
	 * @param message
	 * @return
	 * @throws IOException
	 */
	public static boolean log(String host,
			SyslogLevel level, SyslogFacility facility,
			java.util.Date date, String hostname,
			String message) throws IOException {
		return log(InetAddress.getByName(host), PORT, level,
				facility, date, hostname, message);
	}

	/**
	 * 
	 * @param host
	 * @param level
	 * @param facility
	 * @param hostname
	 * @param message
	 * @return
	 * @throws IOException
	 */
	public static boolean log(String host,
			SyslogLevel level, SyslogFacility facility,
			String hostname, String message) throws IOException {
		return log(InetAddress.getByName(host), PORT, level,
				facility, new java.util.Date(), hostname, message);
	}

}
