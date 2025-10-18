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
package net.morilib.lisp.system;

import net.morilib.lang.system.OSInfo;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.subr.NoArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/23
 */
public class GuessOs extends NoArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.NoArgs#execute(net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Environment env, LispMessage mesg) {
		switch(OSInfo.OS) {
		case LINUX:     return Symbol.getSymbol("linux");
		case AIX:       return Symbol.getSymbol("aix");
		case HP_UX:     return Symbol.getSymbol("hp-ux");
		case MAC_OS_X:  return Symbol.getSymbol("mac-os-x");
		case FREE_BSD:  return Symbol.getSymbol("free-bsd");
		case OPEN_BSD:  return Symbol.getSymbol("open-bsd");
		case NET_BSD:   return Symbol.getSymbol("net-bsd");
		case SOLARIS:   return Symbol.getSymbol("solaris");
		case SUN_OS:    return Symbol.getSymbol("sun-os");
		case OTHER_WINDOWS:  return Symbol.getSymbol("windows");
		case WINDOWS_XP:     return Symbol.getSymbol("windows-xp");
		case WINDOWS_VISTA:  return Symbol.getSymbol("windows-vista");
		case WINDOWS_7:      return Symbol.getSymbol("windows-7");
		case WINDOWS_2003_SERVER:
			return Symbol.getSymbol("windows-2003-server");
		case WINDOWS_2008_SERVER:
			return Symbol.getSymbol("windows-2008-server");
		case CLASSIC_MAC:
			return Symbol.getSymbol("classic-mac");
		default:  return LispBoolean.FALSE;
		}
	}

}
