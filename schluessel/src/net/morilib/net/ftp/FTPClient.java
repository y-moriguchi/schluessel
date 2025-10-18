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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.util.List;

import net.morilib.net.CRLFPrintStream;
import net.morilib.net.NetUtils;
import net.morilib.util.IOs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/14
 */
public abstract class FTPClient {

	//
	/*package*/ InetAddress  address = null;
	/*package*/ int port = 21;
	/*package*/ transient Socket socket = null;
	/*package*/ transient InputStream rd = null;
	/*package*/ transient CRLFPrintStream pr = null;
	/*package*/ transient FTPMode mode;

	/**
	 * 
	 */
	public FTPClient() {
		// do nothing
	}

	/**
	 * 
	 * @param address
	 * @param port
	 * @param clientAddress
	 * @throws IOException 
	 */
	public FTPClient(InetAddress address, int port,
			String username, String password) throws IOException {
		this.address = address;
		this.port    = port;
		connect(address, port, username, password);
	}

	/**
	 * 
	 * @param address
	 * @param clientAddress
	 * @throws IOException
	 */
	public FTPClient(InetAddress address,
			String username, String password) throws IOException {
		this(address, 21, username, password);
	}

	/**
	 * 
	 * @param address
	 * @param port
	 * @throws IOException 
	 */
	public void connect(InetAddress address, int port,
			String username, String password) throws IOException {
		socket = new Socket(address, port);
		rd = socket.getInputStream();
		pr = new CRLFPrintStream(socket.getOutputStream());
		NetUtils.getCode(rd, 200);
		login(username, password);
		setMode(FTPMode.BINARY);
	}

	/**
	 * 
	 */
	public void close() {
		IOs.close(rd);
		IOs.close(pr);
		if(socket != null) {
			try {
				socket.close();
				socket = null;
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
	}

	/**
	 * 
	 * @param username
	 * @param password
	 * @throws IOException
	 */
	public void login(String username,
			String password) throws IOException {
		if(socket == null) {
			throw new IllegalStateException();
		}
		FTP.user(pr, rd, username);
		FTP.pass(pr, rd, password);
	}

	/**
	 * 
	 * @return
	 * @throws IOException
	 */
	public void quit() throws IOException {
		if(socket == null) {
			throw new IllegalStateException();
		}
		FTP.quit(pr, rd);
	}

	/**
	 * 
	 * @return
	 * @throws IOException
	 */
	public String system() throws IOException {
		if(socket == null) {
			throw new IllegalStateException();
		}
		return FTP.syst(pr, rd);
	}

	/**
	 * 
	 * @param filename
	 * @return
	 * @throws IOException
	 */
	public boolean delete(String filename) throws IOException {
		int r;

		if(socket == null) {
			throw new IllegalStateException();
		}
		r = FTP.dele(pr, rd, filename);
		return (r >= 200 && r < 300);
	}

	/**
	 * 
	 * @return
	 * @throws IOException
	 */
	public String pwd() throws IOException {
		if(socket == null) {
			throw new IllegalStateException();
		}
		return FTP.pwd(pr, rd);
	}

	/**
	 * 
	 * @param dir
	 * @return
	 * @throws IOException
	 */
	public boolean cwd(String dir) throws IOException {
		int r;

		if(socket == null) {
			throw new IllegalStateException();
		}
		r = FTP.cwd(pr, rd, dir);
		return (r >= 200 && r < 300);
	}

	/**
	 * 
	 * @param dir
	 * @return
	 * @throws IOException
	 */
	public boolean mkdir(String dir) throws IOException {
		int r;

		if(socket == null) {
			throw new IllegalStateException();
		}
		r = FTP.mkd(pr, rd, dir);
		return (r >= 200 && r < 300);
	}

	/**
	 * 
	 * @param dir
	 * @return
	 * @throws IOException
	 */
	public boolean rmdir(String dir) throws IOException {
		int r;

		if(socket == null) {
			throw new IllegalStateException();
		}
		r = FTP.rmd(pr, rd, dir);
		return (r >= 200 && r < 300);
	}

	/**
	 * 
	 * @param from
	 * @param to
	 * @return
	 * @throws IOException
	 */
	public boolean rename(String from, String to) throws IOException {
		int r;

		if(socket == null) {
			throw new IllegalStateException();
		} else if((r = FTP.rnfr(pr, rd, from)) >= 300 && r < 400) {
			r = FTP.rnto(pr, rd, to);
			return (r >= 200 && r < 300);
		} else {
			return false;
		}
	}

	/**
	 * 
	 * @param mode
	 * @throws IOException
	 */
	public void setMode(FTPMode mode) throws IOException {
		if(socket == null) {
			throw new IllegalStateException();
		}
		FTP.type(pr, rd, mode);
		this.mode = mode;
	}

	/**
	 * 
	 * @param filename
	 * @return
	 * @throws IOException
	 */
	public long getSize(String filename) throws IOException {
		if(socket == null) {
			throw new IllegalStateException();
		}
		return FTP.size(pr, rd, filename);
	}

	/**
	 * 
	 * @param filename
	 * @return
	 * @throws IOException
	 */
	public java.util.Date getTimestamp(
			String filename) throws IOException {
		if(socket == null) {
			throw new IllegalStateException();
		}
		return FTP.mdtm(pr, rd, filename);
	}

	/**
	 * 
	 * @return
	 */
	public boolean isConnected() {
		return socket != null;
	}

	/**
	 * 
	 * @return
	 * @throws IOException
	 */
	public abstract String list() throws IOException;

	/**
	 * 
	 * @return
	 * @throws IOException
	 */
	public abstract List<String> nameList() throws IOException;

	/**
	 * 
	 * @param filename
	 * @param ins
	 * @throws IOException
	 */
	public abstract void put(String filename,
			InputStream ins) throws IOException;

	/**
	 * 
	 * @param filename
	 * @param ous
	 * @throws IOException
	 */
	public abstract void get(String filename,
			OutputStream ous) throws IOException;

	/**
	 * 
	 * @param filename
	 * @param localfile
	 * @throws IOException
	 */
	public void put(String filename,
			File localfile) throws IOException {
		InputStream ins = null;

		try {
			ins = new FileInputStream(localfile);
			put(filename, ins);
		} finally {
			IOs.close(ins);
		}
	}

	/**
	 * 
	 * @param filename
	 * @param localfile
	 * @throws IOException
	 */
	public void get(String filename,
			File localfile) throws IOException {
		OutputStream ous = null;

		try {
			ous = new FileOutputStream(localfile);
			get(filename, ous);
		} finally {
			IOs.close(ous);
		}
	}

	/**
	 * 
	 * @param filename
	 * @param localfile
	 * @throws IOException
	 */
	public void put(String filename,
			String localfile) throws IOException {
		put(filename, new File(localfile));
	}

	/**
	 * 
	 * @param filename
	 * @param localfile
	 * @throws IOException
	 */
	public void get(String filename,
			String localfile) throws IOException {
		get(filename, new File(localfile));
	}

	/**
	 * 
	 * @param filename
	 * @throws IOException
	 */
	public void put(File filename) throws IOException {
		put(filename.getName(), filename);
	}

	/**
	 * 
	 * @param filename
	 * @throws IOException
	 */
	public void get(File filename) throws IOException {
		get(filename.getName(), filename);
	}

	/**
	 * @return the address
	 */
	public InetAddress getAddress() {
		return address;
	}

	/**
	 * @return the port
	 */
	public int getPort() {
		return port;
	}

	/**
	 * @return the mode
	 */
	public FTPMode getMode() {
		return mode;
	}

}
