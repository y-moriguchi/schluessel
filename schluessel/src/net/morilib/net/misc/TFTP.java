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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.ProtocolException;
import java.net.SocketTimeoutException;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/21
 */
public final class TFTP {

	/**
	 * 
	 */
	public static final String NETASCII_MODE = "netascii";

	/**
	 * 
	 */
	public static final String OCTET_MODE = "octet";

	/**
	 * 
	 */
	public static final String MAIL_MODE = "mail";

	/**
	 * 
	 */
	public static final int DEFAULT_PORT = 69;

	//
	private static final short RRQ  = 1;
	private static final short WRQ  = 2;
	private static final short DATA = 3;
	private static final short ACK  = 4;
	private static final short ERR  = 5;
	private static final int DEFAULT_TIMEOUT = 2000;

	//
	private TFTP() {}

	//
	static void putstr(DataOutputStream dos,
			String str) throws IOException {
		dos.write(str.getBytes());
		dos.writeByte(0);
	}

	//
	static byte[] newRWRQ(short rw, String filename, String mode) {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		DataOutputStream dos = new DataOutputStream(bos);

		try {
			if(rw != RRQ && rw != WRQ) {
				throw new IllegalArgumentException();
			} else if(filename == null || mode == null) {
				throw new NullPointerException();
			} else if(filename.equals("") || mode.equals("")) {
				throw new IllegalArgumentException();
			}
			dos.writeShort(rw);
			putstr(dos, filename);
			putstr(dos, mode);
			return bos.toByteArray();
		} catch(IOException e) {
			throw new RuntimeException(e);
		}
	}

	//
	static void throwErrorPacket(
			DataInputStream dis) throws IOException {
		byte[] b = new byte[512];
		int ec, l;

		ec = dis.readUnsignedShort();
		l  = dis.read(b) - 1;
		throw new TFTPErrorPacketException(new String(b, 0, l), ec);
	}

	//
	static int checkAck(DatagramSocket sok, byte[] b,
			int c) throws IOException {
		DatagramPacket rpk;
		ByteArrayInputStream bis;
		DataInputStream dis;
		int cd;

		rpk = new DatagramPacket(b, b.length);
		sok.receive(rpk);
		bis = new ByteArrayInputStream(b, 0, rpk.getLength());
		dis = new DataInputStream(bis);
		if((cd = dis.readShort()) == ERR) {
			throwErrorPacket(dis);
		} else if(cd != ACK) {
			throw new ProtocolException("illegal state");
		} else if(dis.readUnsignedShort() != c) {
			throw new ProtocolException("block number");
		}
		return rpk.getPort();
	}

	//
	static void putdata(DatagramSocket sok, InetAddress addr,
			int port, InputStream ins) throws IOException {
		DatagramPacket spk;
		byte[] b = new byte[516], s = null;
		int l, c = 0;

		while((l = ins.read(b, 4, 512)) >= 0) {
			if(l == 0 || l > 512) {
				continue;
			} else if(l < 512) {
				s = new byte[l + 4];  c++;
				System.arraycopy(b, 0, s, 0, s.length);
			} else if(l == 512) {
				s = b;  c++;
			}
			s[0] = 0;  s[1] = DATA;
			s[2] = (byte)(c >> 8);  s[3] = (byte)(c & 0xff);

			spk = new DatagramPacket(s, s.length, addr, port);
			sok.send(spk);
			checkAck(sok, b, c);
		}
	}

	//
	static long getdata(DatagramSocket sok, InetAddress addr,
			 OutputStream ous) throws IOException {
		ByteArrayInputStream bis;
		DataInputStream dis;
		DatagramPacket spk, rpk;
		byte[] b = new byte[516], s = new byte[4];
		int cd, port;
		long l = 0;

		try {
			for(int c = 1; true; c++) {
				rpk = new DatagramPacket(b, b.length);
				sok.receive(rpk);
				bis = new ByteArrayInputStream(b, 0, rpk.getLength());
				dis = new DataInputStream(bis);

				if((cd = dis.readShort()) == ERR) {
					throwErrorPacket(dis);
				} else if(cd != DATA) {
					throw new ProtocolException("illegal state");
				} else if(dis.readUnsignedShort() != c) {
					throw new ProtocolException("block number");
				}
				ous.write(b, 4, rpk.getLength());
				l += rpk.getLength() - 4;

				port = rpk.getPort();
				s[0] = 0;  s[1] = ACK;
				s[2] = (byte)(c >> 8);  s[3] = (byte)(c & 0xff);
				spk = new DatagramPacket(s, s.length, addr, port);
				sok.send(spk);
			}
		} catch(SocketTimeoutException e) {
			// ignore
		}
		return l;
	}

	/**
	 * 
	 * @param addr
	 * @param port
	 * @param filename
	 * @param mode
	 * @param ins
	 * @throws IOException
	 */
	public static void put(InetAddress addr, int port, String filename,
			String mode, InputStream ins) throws IOException {
		DatagramSocket sok = new DatagramSocket();
		DatagramPacket spk;
		byte[] b;

		sok.setSoTimeout(DEFAULT_TIMEOUT);
		b = newRWRQ(WRQ, filename, mode);
		spk = new DatagramPacket(b, b.length, addr, port);
		sok.send(spk);
		putdata(sok, addr, checkAck(sok, b, 0), ins);
	}

	/**
	 * 
	 * @param host
	 * @param port
	 * @param filename
	 * @param mode
	 * @param ins
	 * @throws IOException
	 */
	public static void put(String host, int port, String filename,
			String mode, InputStream ins) throws IOException {
		put(InetAddress.getByName(host), port, filename, mode, ins);
	}

	/**
	 * 
	 * @param addr
	 * @param filename
	 * @param mode
	 * @param ins
	 * @throws IOException
	 */
	public static void put(InetAddress addr, String filename,
			String mode, InputStream ins) throws IOException {
		put(addr, DEFAULT_PORT, filename, mode, ins);
	}

	/**
	 * 
	 * @param host
	 * @param filename
	 * @param mode
	 * @param ins
	 * @throws IOException
	 */
	public static void put(String host, String filename,
			String mode, InputStream ins) throws IOException {
		put(InetAddress.getByName(host), DEFAULT_PORT, filename, mode,
				ins);
	}

	/**
	 * 
	 * @param addr
	 * @param port
	 * @param filename
	 * @param mode
	 * @param ous
	 * @return
	 * @throws IOException
	 */
	public static long get(InetAddress addr, int port, String filename,
			String mode, OutputStream ous) throws IOException {
		DatagramSocket sok = new DatagramSocket();
		DatagramPacket spk;
		byte[] b;

		sok.setSoTimeout(DEFAULT_TIMEOUT);
		b = newRWRQ(RRQ, filename, mode);
		spk = new DatagramPacket(b, b.length, addr, port);
		sok.send(spk);
		return getdata(sok, addr, ous);
	}

	/**
	 * 
	 * @param host
	 * @param port
	 * @param filename
	 * @param mode
	 * @param ins
	 * @throws IOException
	 */
	public static void get(String host, int port, String filename,
			String mode, OutputStream ous) throws IOException {
		get(InetAddress.getByName(host), port, filename, mode, ous);
	}

	/**
	 * 
	 * @param addr
	 * @param filename
	 * @param mode
	 * @param ins
	 * @throws IOException
	 */
	public static void get(InetAddress addr, String filename,
			String mode, OutputStream ous) throws IOException {
		get(addr, DEFAULT_PORT, filename, mode, ous);
	}

	/**
	 * 
	 * @param host
	 * @param filename
	 * @param mode
	 * @param ins
	 * @throws IOException
	 */
	public static void get(String host, String filename,
			String mode, OutputStream ous) throws IOException {
		get(InetAddress.getByName(host), DEFAULT_PORT, filename, mode,
				ous);
	}

}
