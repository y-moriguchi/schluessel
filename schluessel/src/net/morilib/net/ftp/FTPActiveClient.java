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
package net.morilib.net.ftp;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Inet4Address;
import java.net.InetAddress;
import java.util.List;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/14
 */
public class FTPActiveClient extends FTPClient {

	//
	/*package*/ Inet4Address clientAddress = null;

	/**
	 * 
	 */
	public FTPActiveClient() {
		// do nothing
	}

	/**
	 * 
	 * @param address
	 * @param port
	 * @param clientAddress
	 * @throws IOException 
	 */
	public FTPActiveClient(InetAddress address, int port,
			Inet4Address clientAddress,
			String username, String password) throws IOException {
		super(address, port, username, password);
		this.clientAddress = clientAddress;
	}

	/**
	 * 
	 * @param address
	 * @param clientAddress
	 * @throws IOException
	 */
	public FTPActiveClient(InetAddress address,
			Inet4Address clientAddress,
			String username, String password) throws IOException {
		this(address, 21, clientAddress, username, password);
	}

	/**
	 * 
	 * @return
	 * @throws IOException
	 */
	public String list() throws IOException {
		if(!isConnected()) {
			throw new IllegalStateException();
		}
		return FTP.receiveList(pr, rd, clientAddress);
	}

	/**
	 * 
	 * @return
	 * @throws IOException
	 */
	public List<String> nameList() throws IOException {
		if(!isConnected()) {
			throw new IllegalStateException();
		}
		return FTP.receiveNameList(pr, rd, clientAddress);
	}

	/**
	 * 
	 * @param filename
	 * @param ins
	 * @throws IOException
	 */
	public void put(String filename,
			InputStream ins) throws IOException {
		if(!isConnected()) {
			throw new IllegalStateException();
		}
		FTP.put(pr, rd, filename, clientAddress, ins);
	}

	/**
	 * 
	 * @param filename
	 * @param ous
	 * @throws IOException
	 */
	public void get(String filename,
			OutputStream ous) throws IOException {
		if(!isConnected()) {
			throw new IllegalStateException();
		}
		FTP.get(pr, rd, filename, clientAddress, ous);
	}

	/**
	 * @return the clientAddress
	 */
	public Inet4Address getClientAddress() {
		return clientAddress;
	}

}
