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
package net.morilib.lisp.net.misc;

import java.io.IOException;
import java.net.InetAddress;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.LispString;
import net.morilib.lisp.Subr;
import net.morilib.lisp.net.LispInetAddress;
import net.morilib.lisp.net.SubrNetUtils;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.net.syslog.SyslogFacility;
import net.morilib.net.syslog.SyslogLevel;
import net.morilib.net.syslog.SyslogP;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/18
 */
public class SyslogLogger extends Subr {

	//
	private static final String[] FACILITIES_STR = new String[] {
		"kernel-messages", "user-level-messages", "mail-system",
		"system-daemons", "security-messages", "messages-by-syslogd",
		"line-printer-subsystem", "network-news-subsystem",
		"uucp-subsystem", "clock-daemon", "security-messages2",
		"ftp-daemon", "ntp-subsystem", "log-audit", "log-alert",
		"clock-daemon2", "local0", "local1", "local2", "local3",
		"local4", "local5", "local6", "local7"
	};

	//
	private static final
	SyslogFacility[] FACILITIES = new SyslogFacility[] {
		SyslogFacility.KERNEL_MESSAGES,
		SyslogFacility.USER_LEVEL_MESSAGES,
		SyslogFacility.MAIL_SYSTEM,
		SyslogFacility.SYSTEM_DAEMONS,
		SyslogFacility.SECURITY_MESSAGES,
		SyslogFacility.MESSAGES_BY_SYSLOGD,
		SyslogFacility.LINE_PRINTER_SUBSYSTEM,
		SyslogFacility.NETWORK_NEWS_SUBSYSTEM,
		SyslogFacility.UUCP_SUBSYSTEM,
		SyslogFacility.CLOCK_DAEMON,
		SyslogFacility.SECURITY_MESSAGES2,
		SyslogFacility.FTP_DAEMON,
		SyslogFacility.NTP_SUBSYSTEM,
		SyslogFacility.LOG_AUDIT,
		SyslogFacility.LOG_ALERT,
		SyslogFacility.CLOCK_DAEMON2,
		SyslogFacility.LOCAL0,
		SyslogFacility.LOCAL1,
		SyslogFacility.LOCAL2,
		SyslogFacility.LOCAL3,
		SyslogFacility.LOCAL4,
		SyslogFacility.LOCAL5,
		SyslogFacility.LOCAL6,
		SyslogFacility.LOCAL7,
	};

	//
	private static final String[] LEVELS_STR = new String[] {
		"emergency", "alert", "critical", "error",
		"warning", "notice", "info", "debug"
	};

	//
	private static final SyslogLevel[] LEVELS = new SyslogLevel[] {
		SyslogLevel.EMERGENCY,     SyslogLevel.ALERT,
		SyslogLevel.CRITICAL,      SyslogLevel.ERROR,
		SyslogLevel.WARNING,       SyslogLevel.NOTICE,
		SyslogLevel.INFORMATIONAL, SyslogLevel.DEBUG,
	};

	//
	private SyslogLevel getlevel(String lvl) {
		for(int i = 0; i < LEVELS_STR.length; i++) {
			if(LEVELS_STR[i].equalsIgnoreCase(lvl)) {
				return LEVELS[i];
			}
		}
		return null;
	}

	//
	private SyslogFacility getfacility(String fac) {
		for(int i = 0; i < FACILITIES_STR.length; i++) {
			if(FACILITIES_STR[i].equalsIgnoreCase(fac)) {
				return FACILITIES[i];
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		InetAddress addr;
		Datum d1, d2;
		String facs, lvls, host, lmsg;
		int port = 514;
		SyslogFacility fac;
		SyslogLevel lvl;

		try {
			addr = SubrNetUtils.nextAddress(itr, body, mesg);
			d1   = SubrUtils.nextIf(itr, mesg, body);
			if(d1 instanceof LispSmallInt) {
				port = SubrUtils.getSmallInt(d1, mesg);
				facs = SubrUtils.nextSymbolName(itr, mesg, body);
			} else {
				facs = SubrUtils.getSymbolName(d1, mesg);
			}
			lvls = SubrUtils.nextSymbolName(itr, mesg, body);
			d2   = SubrUtils.nextIf(itr, mesg, body);
	
			if(d2 instanceof LispString) {
				host = SubrUtils.getString(d2, mesg);
			} else if(d2 instanceof LispInetAddress) {
				host = ((LispInetAddress)d2).getAddress().toString();
			} else {
				throw mesg.getError(
						"err.net.require.hostnameoraddress", d2);
			}
			lmsg = SubrUtils.nextString(itr, mesg, body);
			SubrUtils.checkTerminated(itr, body, mesg);

			if((fac = getfacility(facs)) == null) {
				return LispBoolean.FALSE;
			} else if((lvl = getlevel(lvls)) == null) {
				return LispBoolean.FALSE;
			} else {
				return LispBoolean.getInstance(SyslogP.log(addr, port,
						lvl, fac, host, lmsg));
			}
		} catch (IOException e) {
			throw mesg.getError("err.io", e.getMessage());
		}
	}

}
