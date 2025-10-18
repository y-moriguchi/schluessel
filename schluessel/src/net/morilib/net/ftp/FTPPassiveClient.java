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
import java.net.InetAddress;
import java.util.List;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/14
 */
public class FTPPassiveClient extends FTPClient {

	/**
	 * 
	 */
	public FTPPassiveClient() {
		super();
	}

	/**
	 * @param address
	 * @param port
	 * @throws IOException
	 */
	public FTPPassiveClient(InetAddress address, int port,
			String username, String password) throws IOException {
		super(address, port, username, password);
	}

	/**
	 * @param address
	 * @throws IOException
	 */
	public FTPPassiveClient(InetAddress address,
			String username, String password) throws IOException {
		super(address, username, password);
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
		return FTP.receiveListPassive(pr, rd);
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
		return FTP.receiveNameListPassive(pr, rd);
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
		FTP.putPassive(pr, rd, filename, ins);
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
		FTP.getPassive(pr, rd, filename, ous);
	}

}
