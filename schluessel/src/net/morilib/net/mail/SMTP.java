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
package net.morilib.net.mail;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringReader;
import java.net.InetAddress;
import java.net.Socket;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;

import net.morilib.net.CRLFPrintStream;
import net.morilib.net.NetUtils;
import net.morilib.util.codec.MIME;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/12/18
 */
public class SMTP {

	/**
	 * 
	 */
	public static final SimpleDateFormat RFC2822_DATE =
		new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss Z", Locale.US);

	//
	private static List<String> _asList(String[] s) {
		return (s == null) ?
				Collections.<String>emptyList() : Arrays.asList(s);
	}

	//
	/*package*/ static void _send(OutputStream pr, InputStream rd,
			String hostname, String subject, String from,
			List<String> to, List<String> cc, List<String> bcc,
			BufferedReader text, String encoding
			) throws IOException {
		CRLFPrintStream prpr = new CRLFPrintStream(pr);
		String x, dlm;

		NetUtils.getCode(rd, 200);
		helo(prpr, rd, hostname);
		from(prpr, rd, from);
		for(String s : to) {
			rcpt(prpr, rd, s);
		}

		for(String s : cc) {
			rcpt(prpr, rd, s);
		}

		for(String s : bcc) {
			rcpt(prpr, rd, s);
		}
		data(prpr, rd);
		prpr.println("Mime-Version: 1.0");
		if(encoding != null) {
			prpr.print("Content-Type: text/plain; charset=\"");
			prpr.print(encoding);
			prpr.println("\"");
			prpr.println(
					MIME.encodeBase64(subject, "Subject:", encoding));
			prpr.println("Content-Transfer-Encoding: 7bit");
		} else {
			prpr.println("Content-Type: text/plain");
			prpr.print("Subject: ");
			prpr.println(subject);
		}
		prpr.print("From: ");
		prpr.println(from);
		if(to.size() > 0) {
			prpr.print("To: ");
			dlm = "";
			for(String s : to) {
				prpr.print(dlm);
				prpr.print(s);
				dlm = ", ";
			}
			prpr.println("");
		}

		if(cc.size() > 0) {
			prpr.print("Cc: ");
			dlm = "";
			for(String s : cc) {
				prpr.print(dlm);
				prpr.print(s);
				dlm = ", ";
			}
			prpr.println("");
		}

		if(bcc.size() > 0) {
			prpr.print("Bcc: ");
			dlm = "";
			for(String s : bcc) {
				prpr.print(dlm);
				prpr.print(s);
				dlm = ", ";
			}
			prpr.println("");
		}

		prpr.print("Date: ");
		prpr.println(RFC2822_DATE.format(new java.util.Date()));
		while((x = text.readLine()) != null) {
			if(encoding != null) {
				prpr.write(x.getBytes(encoding));
			} else {
				prpr.write(x.getBytes());
			}
			prpr.write('\r');  prpr.write('\n');
		}
		prpr.println(".");
		NetUtils.getCode(rd, 200);
		quit(prpr, rd);
	}

	/**
	 * 
	 * @param smtpname
	 * @param port
	 * @param subject
	 * @param from
	 * @param to
	 * @param cc
	 * @param bcc
	 * @param text
	 * @param encoding
	 * @throws IOException
	 */
	public static void send(String smtpname, int port,
			String subject, String from,
			List<String> to, List<String> cc, List<String> bcc,
			String text, String encoding
			) throws IOException {
		Socket sok = new Socket(smtpname, port);
		BufferedReader rd = new BufferedReader(new StringReader(text));

		_send(sok.getOutputStream(), sok.getInputStream(),
				InetAddress.getLocalHost().getHostName(),
				subject, from, to, cc, bcc, rd, encoding);
	}

	/**
	 * 
	 * @param smtpname
	 * @param port
	 * @param subject
	 * @param from
	 * @param to
	 * @param cc
	 * @param bcc
	 * @param text
	 * @param encoding
	 * @throws IOException
	 */
	public static void send(String smtpname, int port,
			String subject, String from,
			String[] to, String[] cc, String[] bcc,
			String text, String encoding
			) throws IOException {
		Socket sok = new Socket(smtpname, port);
		BufferedReader rd = new BufferedReader(new StringReader(text));

		_send(sok.getOutputStream(), sok.getInputStream(),
				InetAddress.getLocalHost().getHostName(),
				subject, from, _asList(to), _asList(cc), _asList(bcc),
				rd, encoding);
	}

	/**
	 * 
	 * @param smtpname
	 * @param subject
	 * @param from
	 * @param to
	 * @param cc
	 * @param bcc
	 * @param text
	 * @param encoding
	 * @throws IOException
	 */
	public static void send(String smtpname,
			String subject, String from,
			List<String> to, List<String> cc, List<String> bcc,
			String text, String encoding
			) throws IOException {
		Socket sok = new Socket(smtpname, 25);
		BufferedReader rd = new BufferedReader(new StringReader(text));

		_send(sok.getOutputStream(), sok.getInputStream(),
				InetAddress.getLocalHost().getHostName(),
				subject, from, to, cc, bcc, rd, encoding);
	}

	/**
	 * 
	 * @param smtpname
	 * @param port
	 * @param subject
	 * @param from
	 * @param to
	 * @param cc
	 * @param bcc
	 * @param text
	 * @param encoding
	 * @throws IOException
	 */
	public static void send(String smtpname,
			String subject, String from,
			String[] to, String[] cc, String[] bcc,
			String text, String encoding
			) throws IOException {
		Socket sok = new Socket(smtpname, 25);
		BufferedReader rd = new BufferedReader(new StringReader(text));

		_send(sok.getOutputStream(), sok.getInputStream(),
				InetAddress.getLocalHost().getHostName(),
				subject, from, _asList(to), _asList(cc), _asList(bcc),
				rd, encoding);
	}

	/**
	 * 
	 * @param smtpaddr
	 * @param port
	 * @param subject
	 * @param from
	 * @param to
	 * @param cc
	 * @param bcc
	 * @param text
	 * @param encoding
	 * @throws IOException
	 */
	public static void send(InetAddress smtpaddr, int port,
			String subject, String from,
			List<String> to, List<String> cc, List<String> bcc,
			String text, String encoding
			) throws IOException {
		Socket sok = new Socket(smtpaddr, port);
		BufferedReader rd = new BufferedReader(new StringReader(text));

		_send(sok.getOutputStream(), sok.getInputStream(),
				InetAddress.getLocalHost().getHostName(),
				subject, from, to, cc, bcc, rd, encoding);
	}

	/**
	 * 
	 * @param smtpaddr
	 * @param port
	 * @param subject
	 * @param from
	 * @param to
	 * @param cc
	 * @param bcc
	 * @param text
	 * @param encoding
	 * @throws IOException
	 */
	public static void send(InetAddress smtpaddr, int port,
			String subject, String from,
			String[] to, String[] cc, String[] bcc,
			String text, String encoding
			) throws IOException {
		Socket sok = new Socket(smtpaddr, port);
		BufferedReader rd = new BufferedReader(new StringReader(text));

		_send(sok.getOutputStream(), sok.getInputStream(),
				InetAddress.getLocalHost().getHostName(),
				subject, from, _asList(to), _asList(cc), _asList(bcc),
				rd, encoding);
	}

	/**
	 * 
	 * @param smtpaddr
	 * @param subject
	 * @param from
	 * @param to
	 * @param cc
	 * @param bcc
	 * @param text
	 * @param encoding
	 * @throws IOException
	 */
	public static void send(InetAddress smtpaddr,
			String subject, String from,
			List<String> to, List<String> cc, List<String> bcc,
			String text, String encoding
			) throws IOException {
		Socket sok = new Socket(smtpaddr, 25);
		BufferedReader rd = new BufferedReader(new StringReader(text));

		_send(sok.getOutputStream(), sok.getInputStream(),
				InetAddress.getLocalHost().getHostName(),
				subject, from, to, cc, bcc, rd, encoding);
	}

	/**
	 * 
	 * @param smtpaddr
	 * @param subject
	 * @param from
	 * @param to
	 * @param cc
	 * @param bcc
	 * @param text
	 * @param encoding
	 * @throws IOException
	 */
	public static void send(InetAddress smtpaddr,
			String subject, String from,
			String[] to, String[] cc, String[] bcc,
			String text, String encoding
			) throws IOException {
		Socket sok = new Socket(smtpaddr, 25);
		BufferedReader rd = new BufferedReader(new StringReader(text));

		_send(sok.getOutputStream(), sok.getInputStream(),
				InetAddress.getLocalHost().getHostName(),
				subject, from, _asList(to), _asList(cc), _asList(bcc),
				rd, encoding);
	}

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @param hostname
	 * @return
	 * @throws IOException
	 */
	public static int helo(CRLFPrintStream pr, InputStream rd,
			String hostname) throws IOException {
		pr.print("HELO ");
		pr.println(hostname);
		return NetUtils.getCode(rd, 200);
	}

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @param from
	 * @return
	 * @throws IOException
	 */
	public static int from(CRLFPrintStream pr, InputStream rd,
			String from) throws IOException {
		pr.print("MAIL FROM:<");
		pr.print(from);
		pr.println(">");
		return NetUtils.getCode(rd, 200);
	}

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @param to
	 * @return
	 * @throws IOException
	 */
	public static int rcpt(CRLFPrintStream pr, InputStream rd,
			String to) throws IOException {
		pr.print("RCPT TO:<");
		pr.print(to);
		pr.println(">");
		return NetUtils.getCode(rd, 200);
	}

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @return
	 * @throws IOException
	 */
	public static int data(CRLFPrintStream pr, InputStream rd
			) throws IOException {
		pr.println("DATA");
		return NetUtils.getCode(rd, 300);
	}

	/**
	 * 
	 * @param pr
	 * @param rd
	 * @return
	 * @throws IOException
	 */
	public static int quit(CRLFPrintStream pr, InputStream rd
			) throws IOException {
		pr.println("QUIT");
		return NetUtils.getCode(rd, 200);
	}

}
