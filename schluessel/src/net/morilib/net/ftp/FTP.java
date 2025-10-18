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

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.ProtocolException;
import java.net.ServerSocket;
import java.net.Socket;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.List;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.morilib.net.CRLFPrintStream;
import net.morilib.net.NetUtils;
import net.morilib.util.Bytes;
import net.morilib.util.IOs;
import net.morilib.util.Strings;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/14
 */
public final class FTP {

	//
	private static final int TIMEOUT = 10000;
//	private static final int BUFSIZE = 4096;
	private static final int START_PORT = 20000;
	private static final int END_PORT   = 65535;
	private static final SimpleDateFormat FMT = new SimpleDateFormat(
			"yyyyMMddHHmmss");
	private static final Pattern PTN1 = Pattern.compile(
			".*\\(([0-9]+),([0-9]+),([0-9]+),([0-9]+)," +
			"([0-9]+),([0-9]+)\\).*");
	private static final Pattern PTN2 =
		Pattern.compile(".*\"(.*?)\".*");

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @param username
	 * @return
	 * @throws IOException
	 */
	public static int user(CRLFPrintStream pr, InputStream rd,
			String username) throws IOException {
		pr.print("USER ");
		pr.println(username);
		return NetUtils.getCode(rd, 300);
	}

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @param password
	 * @return
	 * @throws IOException
	 */
	public static int pass(CRLFPrintStream pr, InputStream rd,
			String password) throws IOException {
		pr.print("PASS ");
		pr.println(password);
		return NetUtils.getCode(rd, 200);
	}

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @return
	 * @throws IOException
	 */
	public static String syst(CRLFPrintStream pr,
			InputStream rd) throws IOException {
		pr.println("SYST");
		NetUtils.readCode(rd, 200);
		return NetUtils.getReason(rd).trim();
	}

	//
	/*package*/ static int port(CRLFPrintStream pr, InputStream rd,
			Inet4Address addr, int port) throws IOException {
		byte[] bts;

		if(addr == null) {
			throw new NullPointerException();
		} else if(port < 0 || port > 65535) {
			throw new IllegalArgumentException();
		}
		bts = addr.getAddress();
		pr.print("PORT ");
		pr.print(Integer.toString(Bytes.ubyteToInt(bts[0])));
		pr.print(",");
		pr.print(Integer.toString(Bytes.ubyteToInt(bts[1])));
		pr.print(",");
		pr.print(Integer.toString(Bytes.ubyteToInt(bts[2])));
		pr.print(",");
		pr.print(Integer.toString(Bytes.ubyteToInt(bts[3])));
		pr.print(",");
		pr.print(Integer.toString(port >> 8));
		pr.print(",");
		pr.println(Integer.toString(port & 0xff));
		return NetUtils.getCode(rd, 200);
	}

	//
	/*package*/ static int list(CRLFPrintStream pr,
			InputStream rd) throws IOException {
		pr.println("LIST");
		return NetUtils.getCode(rd, 100);
	}

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @param mode
	 * @return
	 * @throws IOException
	 */
	public static int type(CRLFPrintStream pr,
			InputStream rd, FTPMode mode) throws IOException {
		pr.print("TYPE ");
		pr.println(mode.getType());
		return NetUtils.getCode(rd, 200);
	}

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @return
	 * @throws IOException
	 */
	public static int typeAscii(CRLFPrintStream pr,
			InputStream rd) throws IOException {
		pr.println("TYPE A");
		return NetUtils.getCode(rd, 200);
	}

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @return
	 * @throws IOException
	 */
	public static int typeBinary(CRLFPrintStream pr,
			InputStream rd) throws IOException {
		pr.println("TYPE I");
		return NetUtils.getCode(rd, 200);
	}

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @return
	 * @throws IOException
	 */
	public static int quit(CRLFPrintStream pr,
			InputStream rd) throws IOException {
		pr.println("QUIT");
		return NetUtils.getCode(rd, 200);
	}

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @param password
	 * @return
	 * @throws IOException
	 */
	public static int mkd(CRLFPrintStream pr, InputStream rd,
			String newdir) throws IOException {
		pr.print("MKD ");
		pr.println(newdir);
		return NetUtils.getCode(rd);
	}

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @param password
	 * @return
	 * @throws IOException
	 */
	public static int rmd(CRLFPrintStream pr, InputStream rd,
			String deldir) throws IOException {
		pr.print("RMD ");
		pr.println(deldir);
		return NetUtils.getCode(rd);
	}

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @param password
	 * @return
	 * @throws IOException
	 */
	public static int rnfr(CRLFPrintStream pr, InputStream rd,
			String renameFrom) throws IOException {
		pr.print("RNFR ");
		pr.println(renameFrom);
		return NetUtils.getCode(rd);
	}

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @param password
	 * @return
	 * @throws IOException
	 */
	public static int rnto(CRLFPrintStream pr, InputStream rd,
			String renameTo) throws IOException {
		pr.print("RNTO ");
		pr.println(renameTo);
		return NetUtils.getCode(rd);
	}

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @param filename
	 * @return
	 * @throws IOException
	 */
	public static int dele(CRLFPrintStream pr, InputStream rd,
			String filename) throws IOException {
		pr.print("DELE ");
		pr.println(filename);
		return NetUtils.getCode(rd);
	}

	//
	/*package*/ static ServerSocket getServerSocket(
			InetAddress addr) {
		for(int i = START_PORT; i <= END_PORT; i++) {
			try {
				return new ServerSocket(i, 0, addr);
			} catch(IOException e) {
				// ignore
			}
		}
		return null;
	}

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @param filename
	 * @return
	 * @throws IOException
	 */
	public static long size(CRLFPrintStream pr,
			InputStream rd, String filename) throws IOException {
		long l = 0;
		int c;

		pr.print("SIZE ");
		pr.println(filename);
		NetUtils.readCode(rd, 200);
		while((c = rd.read()) >= 0 && Character.isWhitespace(c));
		for(; c >= 0 && c >= '0' && c <= '9'; c = rd.read()) {
			l = l * 10 + (c - '0');
		}
		if(c != '\r' || rd.read() != '\n') {
			throw new ProtocolException("bad time");
		}
		return l;
	}

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @param filename
	 * @return
	 * @throws IOException
	 */
	public static java.util.Date mdtm(CRLFPrintStream pr,
			InputStream rd, String filename) throws IOException {
		StringBuilder b = new StringBuilder();
		int c;

		pr.print("MDTM ");
		pr.println(filename);
		NetUtils.readCode(rd, 200);
		while((c = rd.read()) >= 0 && Character.isWhitespace(c));
		for(; c >= 0 && c >= '0' && c <= '9'; c = rd.read()) {
			b.append((char)c);
		}
		if(c != '\r' || rd.read() != '\n') {
			throw new ProtocolException("bad time");
		}

		try {
			return FMT.parse(b.toString());
		} catch (ParseException e) {
			throw new ProtocolException("parse");
		}
	}

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @return
	 * @throws IOException
	 */
	public static String pwd(CRLFPrintStream pr,
			InputStream rd) throws IOException {
		Matcher s;

		pr.println("PWD");
		NetUtils.readCode(rd, 200);
		s = PTN2.matcher(NetUtils.getReason(rd));
		return s.matches() ? s.group(1) : null;
	}

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @param dir
	 * @return
	 * @throws IOException
	 */
	public static int cwd(CRLFPrintStream pr, InputStream rd,
			String dir) throws IOException {
		pr.print("CWD ");
		pr.println(dir);
		return NetUtils.getCode(rd);
	}

	/**
	 * 
	 * @param addr
	 * @param port
	 * @return
	 * @throws IOException
	 */
	public static String receiveList(CRLFPrintStream pr,
			InputStream rd, Inet4Address addr) throws IOException {
		ServerSocket sso = null;
		Socket sok = null;
		BufferedInputStream   bis = null;
		ByteArrayOutputStream b = new ByteArrayOutputStream();

		try {
			if((sso = getServerSocket(addr)) == null) {
				throw new IOException("no free ports");
			}
			sso.setSoTimeout(TIMEOUT);
			port(pr, rd, addr, sso.getLocalPort());
			list(pr, rd);
			sok = sso.accept();
			bis = new BufferedInputStream(sok.getInputStream());
			IOs.copy(bis, b);
			NetUtils.getCode(rd, 200);
		} finally {
			IOs.close(bis);
			if(sok != null) {
				sok.close();
			}
			if(sso != null) {
				sso.close();
			}
		}
		return new String(b.toByteArray(), "UTF-8");
	}

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @param addr
	 * @return
	 * @throws IOException
	 */
	public static List<String> receiveNameList(CRLFPrintStream pr,
			InputStream rd, Inet4Address addr) throws IOException {
		ServerSocket sso = null;
		Socket sok = null;
		BufferedInputStream   bis = null;
		ByteArrayOutputStream b = new ByteArrayOutputStream();

		try {
			if((sso = getServerSocket(addr)) == null) {
				throw new IOException("no free ports");
			}
			sso.setSoTimeout(TIMEOUT);
			port(pr, rd, addr, sso.getLocalPort());
			pr.println("NLST");
			NetUtils.getCode(rd, 100);
			sok = sso.accept();
			bis = new BufferedInputStream(sok.getInputStream());
			IOs.copy(bis, b);
			NetUtils.getCode(rd, 200);
		} finally {
			IOs.close(bis);
			if(sok != null) {
				sok.close();
			}
			if(sso != null) {
				sso.close();
			}
		}
		return Strings.toList(new StringTokenizer(
				new String(b.toByteArray(), "UTF-8"),
				"\r\n"));		
	}

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @param filename
	 * @param addr
	 * @param ous
	 * @return
	 * @throws IOException
	 */
	public static int get(CRLFPrintStream pr, InputStream rd,
			String filename, Inet4Address addr,
			OutputStream ous) throws IOException {
		ServerSocket sso = null;
		Socket sok = null;
		BufferedInputStream  bis = null;
		BufferedOutputStream bos = new BufferedOutputStream(ous);

		try {
			if((sso = getServerSocket(addr)) == null) {
				throw new IOException("no free ports");
			}
			sso.setSoTimeout(TIMEOUT);
			port(pr, rd, addr, sso.getLocalPort());
			pr.print("RETR ");
			pr.println(filename);
			NetUtils.getCode(rd, 100);
			sok = sso.accept();
			bis = new BufferedInputStream(sok.getInputStream());
			IOs.copy(bis, bos);
			bos.flush();
		} finally {
			IOs.close(bis);
			if(sok != null) {
				sok.close();
			}
			if(sso != null) {
				sso.close();
			}
		}
		return NetUtils.getCode(rd, 200);
	}

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @param filename
	 * @param addr
	 * @param ins
	 * @return
	 * @throws IOException
	 */
	public static int put(CRLFPrintStream pr, InputStream rd,
			String filename, Inet4Address addr,
			InputStream ins) throws IOException {
		ServerSocket sso = null;
		Socket sok = null;
		BufferedInputStream  bis = new BufferedInputStream(ins);
		BufferedOutputStream bos = null;

		try {
			if((sso = getServerSocket(addr)) == null) {
				throw new IOException("no free ports");
			}
			sso.setSoTimeout(TIMEOUT);
			port(pr, rd, addr, sso.getLocalPort());
			pr.print("STOR ");
			pr.println(filename);
			NetUtils.getCode(rd, 100);
			sok = sso.accept();
			bos = new BufferedOutputStream(sok.getOutputStream());
			IOs.copy(bis, bos);
			bos.flush();
		} finally {
			IOs.close(bos);
			if(sok != null) {
				sok.close();
			}
			if(sso != null) {
				sso.close();
			}
		}
		return NetUtils.getCode(rd, 200);
	}

	//
	/*package*/ static InetSocketAddress getAddr(
			InputStream rd) throws IOException {
		int r, ok = 200, prt;
		Matcher mt;
		byte[] bts = new byte[4];

		r  = NetUtils.readnum(rd) * 100;
		r += NetUtils.readnum(rd) * 10;
		r += NetUtils.readnum(rd);
		if(r < ok || r > (ok + 99)) {
			throw new ProtocolException(r + NetUtils.getReason(rd));
		} else {
			mt = PTN1.matcher(NetUtils.getReason(rd));
			if(mt.matches()) {
				bts[0] = (byte)Integer.parseInt(mt.group(1));
				bts[1] = (byte)Integer.parseInt(mt.group(2));
				bts[2] = (byte)Integer.parseInt(mt.group(3));
				bts[3] = (byte)Integer.parseInt(mt.group(4));
				prt    = Integer.parseInt(mt.group(5)) << 8;
				prt   |= Integer.parseInt(mt.group(6));
				return new InetSocketAddress(
						InetAddress.getByAddress(bts), prt);
			} else {
				throw new ProtocolException("unknown port");
			}
		}
	}

	/**
	 * 
	 * @param addr
	 * @param port
	 * @return
	 * @throws IOException
	 */
	public static String receiveListPassive(CRLFPrintStream pr,
			InputStream rd) throws IOException {
		Socket sok = new Socket();
		BufferedInputStream   bis = null;
		ByteArrayOutputStream b = new ByteArrayOutputStream();
		InetSocketAddress paddr;

		try {
			pr.println("PASV");
			paddr = getAddr(rd);
			pr.println("LIST");
			sok.connect(paddr);
			NetUtils.getCode(rd, 100);
			bis = new BufferedInputStream(sok.getInputStream());
			IOs.copy(bis, b);
			NetUtils.getCode(rd, 200);
		} finally {
			IOs.close(bis);
			if(sok != null) {
				sok.close();
			}
		}
		return new String(b.toByteArray(), "UTF-8");
	}

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @return
	 * @throws IOException
	 */
	public static List<String> receiveNameListPassive(
			CRLFPrintStream pr, InputStream rd) throws IOException {
		Socket sok = new Socket();
		BufferedInputStream   bis = null;
		ByteArrayOutputStream b = new ByteArrayOutputStream();
		InetSocketAddress paddr;

		try {
			pr.println("PASV");
			paddr = getAddr(rd);
			pr.println("NLST");
			sok.connect(paddr);
			NetUtils.getCode(rd, 100);
			bis = new BufferedInputStream(sok.getInputStream());
			IOs.copy(bis, b);
			NetUtils.getCode(rd, 200);
		} finally {
			IOs.close(bis);
			if(sok != null) {
				sok.close();
			}
		}
		return Strings.toList(new StringTokenizer(
				new String(b.toByteArray(), "UTF-8"), "\r\n"));
	}

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @param filename
	 * @param addr
	 * @param ous
	 * @return
	 * @throws IOException
	 */
	public static int getPassive(CRLFPrintStream pr, InputStream rd,
			String filename, OutputStream ous) throws IOException {
		ServerSocket sso = null;
		Socket sok = new Socket();
		BufferedInputStream  bis = null;
		BufferedOutputStream bos = new BufferedOutputStream(ous);
		InetSocketAddress paddr;

		try {
			pr.println("PASV");
			paddr = getAddr(rd);
			pr.print("RETR ");
			pr.println(filename);
			sok.connect(paddr);
			NetUtils.getCode(rd, 100);
			bis = new BufferedInputStream(sok.getInputStream());
			IOs.copy(bis, bos);
			bos.flush();
		} finally {
			IOs.close(bis);
			if(sok != null) {
				sok.close();
			}
			if(sso != null) {
				sso.close();
			}
		}
		return NetUtils.getCode(rd, 200);
	}

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @param filename
	 * @param addr
	 * @param ins
	 * @return
	 * @throws IOException
	 */
	public static int putPassive(CRLFPrintStream pr, InputStream rd,
			String filename, InputStream ins) throws IOException {
		ServerSocket sso = null;
		Socket sok = new Socket();
		BufferedInputStream  bis = new BufferedInputStream(ins);
		BufferedOutputStream bos = null;
		InetSocketAddress paddr;

		try {
			pr.println("PASV");
			paddr = getAddr(rd);
			pr.print("STOR ");
			pr.println(filename);
			sok.connect(paddr);
			NetUtils.getCode(rd, 100);
			bos = new BufferedOutputStream(sok.getOutputStream());
			IOs.copy(bis, bos);
			bos.flush();
		} finally {
			IOs.close(bos);
			if(sok != null) {
				sok.close();
			}
			if(sso != null) {
				sso.close();
			}
		}
		return NetUtils.getCode(rd, 200);
	}

}
