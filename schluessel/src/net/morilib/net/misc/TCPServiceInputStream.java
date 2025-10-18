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
import java.net.InetAddress;
import java.net.Socket;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/15
 */
public class TCPServiceInputStream extends InputStream {

	/**
	 * 
	 */
	protected Socket sok;

	/**
	 * 
	 */
	protected InputStream ins;

	/**
	 * 
	 * @param address
	 * @param port
	 * @throws IOException
	 */
	public TCPServiceInputStream(InetAddress address,
			int port) throws IOException {
		sok = new Socket(address, port);
		ins = sok.getInputStream();
	}

	/**
	 * 
	 * @param address
	 * @return
	 * @throws IOException
	 */
	public TCPServiceInputStream chargen(
			InetAddress address) throws IOException {
		return new TCPServiceInputStream(address, 19);
	}

	/**
	 * 
	 * @param address
	 * @return
	 * @throws IOException
	 */
	public TCPServiceInputStream netstat(
			InetAddress address) throws IOException {
		return new TCPServiceInputStream(address, 19);
	}

	/**
	 * 
	 * @param address
	 * @return
	 * @throws IOException
	 */
	public TCPServiceInputStream quoteOfDay(
			InetAddress address) throws IOException {
		return new TCPServiceInputStream(address, 17);
	}

	/**
	 * 
	 * @param address
	 * @return
	 * @throws IOException
	 */
	public TCPServiceInputStream systat(
			InetAddress address) throws IOException {
		return new TCPServiceInputStream(address, 11);
	}

	/**
	 * 
	 * @param address
	 * @return
	 * @throws IOException
	 */
	public TCPServiceInputStream daytime(
			InetAddress address) throws IOException {
		return new TCPServiceInputStream(address, 13);
	}

	/* (non-Javadoc)
	 * @see java.io.InputStream#available()
	 */
	@Override
	public int available() throws IOException {
		return ins.available();
	}

	/* (non-Javadoc)
	 * @see java.io.InputStream#close()
	 */
	@Override
	public void close() throws IOException {
//		ins.close();
//		sok.getOutputStream().close();
		sok.close();
	}

	/* (non-Javadoc)
	 * @see java.io.InputStream#read(byte[], int, int)
	 */
	@Override
	public int read(byte[] b, int off, int len) throws IOException {
		return ins.read(b, off, len);
	}

	/* (non-Javadoc)
	 * @see java.io.InputStream#read(byte[])
	 */
	@Override
	public int read(byte[] b) throws IOException {
		return ins.read(b);
	}

	/* (non-Javadoc)
	 * @see java.io.InputStream#skip(long)
	 */
	@Override
	public long skip(long n) throws IOException {
		return ins.skip(n);
	}

	/* (non-Javadoc)
	 * @see java.io.InputStream#read()
	 */
	@Override
	public int read() throws IOException {
		return ins.read();
	}

}
