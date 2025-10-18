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
package net.morilib.net.misc;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/21
 */
public class Echo {

	//
	private static final String HELLO_STR  = "\"HELLO!!\"\n";
	private static final String HELLO_STR2 = "\"HELLO!!\"\n";
	private static final int DEFAULT_TIMEOUT = 2000;
	private static final int BUFSIZE = 4096;

	/**
	 * 
	 */
	public static final int DEFAULT_PORT = 7;

	/**
	 * 
	 * @param addr
	 * @param port
	 * @param s
	 * @return
	 * @throws IOException
	 */
	public static String sendByTCP(InetAddress addr, int port,
			int timeout, String s) throws IOException {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		Socket sok = null;
		InputStream ins = null;
		OutputStream ous = null;
		byte[] buf, bf2 = new byte[BUFSIZE];
		int l;

		try {
			sok = new Socket(addr, port);
			sok.setSoTimeout(timeout);
			ous = sok.getOutputStream();
			if((buf = s.getBytes()).length >= BUFSIZE) {
				throw new IllegalArgumentException();
			}
			ous.write(buf);
			ins = sok.getInputStream();
			l = ins.read(bf2);
			bos.write(bf2, 0, l);
			return new String(bos.toByteArray());
		} finally {
			if(ins != null)  ins.close();
			if(ous != null)  ous.close();
			if(sok != null)  sok.close();
		}
	}

	/**
	 * 
	 * @param host
	 * @param port
	 * @param s
	 * @return
	 * @throws IOException
	 */
	public static String sendByTCP(String host, int port,
			int timeout, String s) throws IOException {
		return sendByTCP(InetAddress.getByName(host), port, timeout,
				s);
	}

	/**
	 * 
	 * @param addr
	 * @param s
	 * @return
	 * @throws IOException
	 */
	public static String sendByTCP(InetAddress addr, int port,
			String s) throws IOException {
		return sendByTCP(addr, port, DEFAULT_TIMEOUT, s);
	}

	/**
	 * 
	 * @param host
	 * @param s
	 * @return
	 * @throws IOException
	 */
	public static String sendByTCP(String host, int port,
			String s) throws IOException {
		return sendByTCP(InetAddress.getByName(host), port,
				DEFAULT_TIMEOUT, s);
	}

	/**
	 * 
	 * @param addr
	 * @param port
	 * @param s
	 * @return
	 * @throws IOException
	 */
	public static String sendByUDP(InetAddress addr, int port,
			int timeout, String s) throws IOException {
		DatagramSocket sok;
		DatagramPacket spk, rpk;
		byte[] b = new byte[BUFSIZE], a = s.getBytes();

		if(a.length >= BUFSIZE) {
			throw new IllegalArgumentException();
		}
		sok = new DatagramSocket();
		sok.setSoTimeout(timeout);
		spk = new DatagramPacket(a, a.length, addr, port);
		sok.send(spk);
		rpk = new DatagramPacket(b, b.length);
		sok.receive(rpk);
		return new String(b, 0, rpk.getLength());
	}

	/**
	 * 
	 * @param host
	 * @param port
	 * @param s
	 * @return
	 * @throws IOException
	 */
	public static String sendByUDP(String host, int port,
			int timeout, String s) throws IOException {
		return sendByUDP(InetAddress.getByName(host), port, timeout,
				s);
	}

	/**
	 * 
	 * @param addr
	 * @param s
	 * @return
	 * @throws IOException
	 */
	public static String sendByUDP(InetAddress addr, int port,
			String s) throws IOException {
		return sendByUDP(addr, port, DEFAULT_TIMEOUT, s);
	}

	/**
	 * 
	 * @param host
	 * @param s
	 * @return
	 * @throws IOException
	 */
	public static String sendByUDP(String host, int port,
			String s) throws IOException {
		return sendByUDP(InetAddress.getByName(host), port,
				DEFAULT_TIMEOUT, s);
	}

	/**
	 * 
	 * @param addr
	 * @param port
	 * @return
	 */
	public static boolean helloTCP(InetAddress addr, int port,
			int timeout) {
		try {
			String s = sendByTCP(addr, port, HELLO_STR);

			return s.equals(HELLO_STR) || s.equals(HELLO_STR2);
		} catch (IOException e) {
			return false;
		}
	}

	/**
	 * 
	 * @param host
	 * @param port
	 * @return
	 */
	public static boolean helloTCP(String host, int port,
			int timeout) {
		try {
			return helloTCP(InetAddress.getByName(host), port,
					timeout);
		} catch (UnknownHostException e) {
			return false;
		}
	}

	/**
	 * 
	 * @param addr
	 * @param port
	 * @return
	 */
	public static boolean helloUDP(InetAddress addr, int port,
			int timeout) {
		try {
			String s = sendByUDP(addr, port, HELLO_STR);

			return s.equals(HELLO_STR);
		} catch (IOException e) {
			return false;
		}
	}

	/**
	 * 
	 * @param host
	 * @param port
	 * @return
	 */
	public static boolean helloUDP(String host, int port,
			int timeout) {
		try {
			return helloUDP(InetAddress.getByName(host), port,
					timeout);
		} catch (UnknownHostException e) {
			return false;
		}
	}

}
