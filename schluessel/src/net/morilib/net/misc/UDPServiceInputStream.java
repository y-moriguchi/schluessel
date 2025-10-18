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

import java.io.IOException;
import java.io.InputStream;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/15
 */
public class UDPServiceInputStream extends InputStream {

	//
	private static final int BUFSIZE = 1024;

	//
	private DatagramSocket sok;
	private DatagramPacket emptyPkt, pkt;
	private byte[] buf;
	private int ptr, maxlength;

	/**
	 * 
	 * @param address
	 * @param port
	 * @throws IOException
	 */
	public UDPServiceInputStream(InetAddress address,
			int port) throws IOException {
		buf = new byte[BUFSIZE];
		sok = new DatagramSocket();
		pkt = new DatagramPacket(buf, buf.length);
		emptyPkt = new DatagramPacket(new byte[0], 0, address, port);
		reload();
	}

	/**
	 * 
	 * @param address
	 * @return
	 * @throws IOException
	 */
	public UDPServiceInputStream chargen(
			InetAddress address) throws IOException {
		return new UDPServiceInputStream(address, 19);
	}

	/**
	 * 
	 * @param address
	 * @return
	 * @throws IOException
	 */
	public UDPServiceInputStream netstat(
			InetAddress address) throws IOException {
		return new UDPServiceInputStream(address, 19);
	}

	/**
	 * 
	 * @param address
	 * @return
	 * @throws IOException
	 */
	public UDPServiceInputStream quoteOfDay(
			InetAddress address) throws IOException {
		return new UDPServiceInputStream(address, 17);
	}

	/**
	 * 
	 * @param address
	 * @return
	 * @throws IOException
	 */
	public UDPServiceInputStream statat(
			InetAddress address) throws IOException {
		return new UDPServiceInputStream(address, 11);
	}

	//
	private void reload() throws IOException {
		sok.send(emptyPkt);
		sok.receive(pkt);
		ptr = 0;
		maxlength = pkt.getLength();
	}

	/* (non-Javadoc)
	 * @see java.io.InputStream#read()
	 */
	@Override
	public int read() throws IOException {
		int r = buf[ptr++];

		if(ptr >= maxlength) {
			reload();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.io.InputStream#available()
	 */
	@Override
	public int available() throws IOException {
		return maxlength - ptr;
	}

	/* (non-Javadoc)
	 * @see java.io.InputStream#close()
	 */
	@Override
	public void close() throws IOException {
		sok.close();
	}

	/* (non-Javadoc)
	 * @see java.io.InputStream#read(byte[], int, int)
	 */
	@Override
	public int read(byte[] b, int off, int len) throws IOException {
		int l = len;

		while(true) {
			if(l < available()) {
				System.arraycopy(buf, ptr, b, off, l);
				ptr += l;
				return len;
			} else {
				System.arraycopy(buf, ptr, b, off, available());
				l -= available();
				reload();
			}
		}
	}

	/* (non-Javadoc)
	 * @see java.io.InputStream#read(byte[])
	 */
	@Override
	public int read(byte[] b) throws IOException {
		return read(b, 0, b.length);
	}

	/* (non-Javadoc)
	 * @see java.io.InputStream#skip(long)
	 */
	@Override
	public long skip(long n) throws IOException {
		long l = n;

		while(true) {
			if(l < available()) {
				ptr += l;
				return n;
			} else {
				l -= available();
				reload();
			}
		}
	}

}
