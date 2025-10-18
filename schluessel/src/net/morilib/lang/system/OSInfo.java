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
package net.morilib.lang.system;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/23
 */
public enum OSInfo {

	UNKNOWN(0),
	LINUX(0x100), AIX(0x101), HP_UX(0x102), MAC_OS_X(0x103),
	FREE_BSD(0x140), OPEN_BSD(0x141), NET_BSD(0x142),
	SOLARIS(0x180), SUN_OS(0x181),
	OTHER_WINDOWS(0x200), WINDOWS_XP(0x201),
	WINDOWS_VISTA(0x202), WINDOWS_7(0x203),
	WINDOWS_2003_SERVER(0x280), WINDOWS_2008_SERVER(0x281),
	CLASSIC_MAC(0x300);

	//
	public static final OSInfo OS;

	//
	static {
		String osn = System.getProperty("os.name");
		String osv = System.getProperty("os.version");

		if(osn.startsWith("Linux") || osn.startsWith("LINUX")) {
			OS = OSInfo.LINUX;
		} else if(osn.startsWith("AIX")) {
			OS = OSInfo.AIX;
		} else if(osn.startsWith("HP-UX")) {
			OS = OSInfo.HP_UX;
		} else if(osn.startsWith("Mac OS X")) {
			OS = OSInfo.MAC_OS_X;
		} else if(osn.startsWith("Mac")) {
			OS = OSInfo.CLASSIC_MAC;
		} else if(osn.startsWith("FreeBSD")) {
			OS = OSInfo.FREE_BSD;
		} else if(osn.startsWith("OpenBSD")) {
			OS = OSInfo.OPEN_BSD;
		} else if(osn.startsWith("NetBSD")) {
			OS = OSInfo.NET_BSD;
		} else if(osn.startsWith("Solaris")) {
			OS = OSInfo.SOLARIS;
		} else if(osn.startsWith("SunOS")) {
			OS = OSInfo.SUN_OS;
		} else if(osn.startsWith("Windows Server 2008")) {
			OS = OSInfo.WINDOWS_2008_SERVER;
		} else if(osn.startsWith("Windows")) {
			if(osv.startsWith("5.1")) {
				OS = OSInfo.WINDOWS_XP;
			} else if(osv.startsWith("5.2")) {
				OS = OSInfo.WINDOWS_2003_SERVER;
			} else if(osv.startsWith("6.0")) {
				OS = OSInfo.WINDOWS_VISTA;
			} else if(osv.startsWith("6.1")) {
				OS = OSInfo.WINDOWS_7;
			} else {
				OS = OSInfo.OTHER_WINDOWS;
			}
		} else {
			OS = OSInfo.UNKNOWN;
		}
	}

	//
	private int code;

	//
	private OSInfo(int code) {
		this.code = code;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isUNIX() {
		return (code & 0xff00) == 0x0100;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isWindows() {
		return (code & 0xff00) == 0x0200;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isWindowsServer() {
		return isWindows() && (code & 0x80) == 0x80;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isClassicMac() {
		return (code & 0xff00) == 0x0300;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isMac() {
		return isClassicMac() || equals(MAC_OS_X);
	}

	/**
	 * 
	 * @return
	 */
	public boolean isBSD() {
		return (code & 0xc0) == 0x40;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isSunOS() {
		return (code & 0xc0) == 0x80;
	}

	/**
	 * 
	 * @return
	 */
	public boolean hasShell() {
		return isUNIX() || isWindows();
	}

	/**
	 * 
	 * @return
	 */
	public String getOptionString() {
		if(isUNIX()) {
			return "-";
		} else if(isWindows()) {
			return "/";
		} else {
			return null;
		}
	}

	/**
	 * 
	 * @return
	 */
	public String getStandardShell() {
		if(equals(LINUX)) {
			return "bash";
		} else if(isUNIX()) {
			return "sh";
		} else if(isWindows()) {
			return "cmd";
		} else {
			return null;
		}
	}

}
