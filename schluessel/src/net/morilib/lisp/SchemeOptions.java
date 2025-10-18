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
package net.morilib.lisp;

import java.io.IOException;
import java.io.StringReader;
import java.util.LinkedHashSet;
import java.util.Locale;
import java.util.Set;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/24
 */
public final class SchemeOptions {

	//
	private static final int ALL_LIBS = 0;
	private static final int R5RS = 1;
//	private static final int NULL_ENV = 2;
	private static final int SELECT_LIBS = 3;
	private static final int INIT = 1000;
	private static final int EVAL = 1010;
	private static final int EXEC = 1020;
	private static final int LOCALE = 1030;
	private static final int LIB_SELECT = 1040;
	private static final String REPORT_LOAD_TIME =
			"--report-load-time";

	//
	private static long loadTime;

	/**
	 * 
	 */
	public static final String SCHLUSH_VERSION = "0.4.2";

	//
	private SchemeOptions() {}

	//
	static Locale parseLocale(String s) {
		String[] ls;

		if(s == null || s.equals(""))  return null;
		ls = s.split("(-|\\.|[_,])");
		switch(ls.length) {
		case 1:
			return new Locale(ls[0].toLowerCase());
		case 2:
			return new Locale(
					ls[0].toLowerCase(),
					ls[1].toUpperCase());
		case 3:
			return new Locale(
					ls[0].toLowerCase(),
					ls[1].toUpperCase(),
					ls[2].toUpperCase());
		default:
			return null;
		}
	}

	/**
	 * @param opt2
	 * @return
	 */
	public static Scheme preparseOption(String[] args) {
		int opt = 0, scm = ALL_LIBS, state = INIT;
		Set<String> libs = new LinkedHashSet<String>();
		String[] a;
		String s, t, u;
		Locale l = null;

		// initialize
		libs.add("core");

		// parse properties (pass 1)
		try {
			l = parseLocale(System.getProperty("schluessel.locale"));
			u = System.getProperty("schluessel.r5rs");
			t = System.getProperty("schluessel.import");
			if(Boolean.parseBoolean(u)) {
				scm = R5RS;
			} else if(t != null && !t.equals("")) {
				a   = t.split(",");
				scm = SELECT_LIBS;
				for(String z : a) {
					if(z.equals("all"))  scm = ALL_LIBS;
					libs.add(z);
				}
			}
		} catch(SecurityException e) {
			// ignore
		}

		// parse options (pass 1)
		outer: while(opt < args.length) {
			s = args[opt++];
			switch(state) {
			case INIT:
				if(s.equals("-version") || s.equals("--version")) {
					System.out.print("Schluessel, Version ");
					System.out.println(SCHLUSH_VERSION);
					System.exit(0);
				} else if(s.equalsIgnoreCase("--r5rs")) {
					scm = R5RS;
				} else if(s.equals("--locale")) {
					state = LOCALE;
				} else if(s.equals("--import")) {
					scm = SELECT_LIBS;
					state = LIB_SELECT;
//				} else if(s.equals("--import-all")) {
//					scm = ALL_LIBS;
				} else if(s.equals(REPORT_LOAD_TIME)) {
					loadTime = System.currentTimeMillis();
				} else if(s.equals("--load-procedure-eager")) {
					IntLispUtils.loadEager = true;
				} else if(s.equals("--")) {
					break outer;
				}
				break;
			case LOCALE:
				if((l = parseLocale(s)) == null) {
					System.err.print("Invalid locale: ");
					System.err.println(s);
					System.exit(2);
				}
				state = INIT;
				break;
			case LIB_SELECT:
				if(s.indexOf('-') == 0) {
					opt--;
					state = INIT;
				} else if(s.equals("all")) {
					scm = ALL_LIBS;
				} else {
					libs.add(s);
				}
				break;
			}
		}

		switch(scm) {
		case ALL_LIBS:    return Scheme.newInstance(l);
		case R5RS:        return Scheme.newRnRS(5, l);
//		case NULL_ENV:    return Scheme.newNull(5, l);
		case SELECT_LIBS:
			try {
				return Scheme.newInstance(
						libs.toArray(new String[0]), l);
			} catch (InitLoadException e) {
				System.err.println(e.getMessage());
				System.exit(2);
			}
		default:  throw new RuntimeException();
		}
	}

	/**
	 * 
	 * @param args
	 * @return
	 */
	public static int parseOption(String[] args, Scheme eval) {
		boolean end = false;
		String s, u;
		long endtime;
		int opt = 0, state = INIT;

		// parse properties (pass 1)
		try {
			u = System.getProperty("schluessel.foldcase");
			if(Boolean.parseBoolean(u)) {
				Symbol.foldCase = Symbol.LOWER_FOLD_CASE;
			} else {
				Symbol.foldCase = Symbol.NO_FOLD_CASE;
			}
		} catch(SecurityException e) {
			// ignore
		}

		// parse options (pass 2)
		try {
			while(opt < args.length) {
				s = args[opt++];
				switch(state) {
				case INIT:
					if(s.equals("-e") || s.equals("--eval")) {
						state = EVAL;
					} else if(s.equals("-E") || s.equals("--exec")) {
						state = EXEC;
					} else if(s.equals("--fold-case")) {
						Symbol.foldCase = Symbol.LOWER_FOLD_CASE;
					} else if(s.equals("--no-fold-case")) {
						Symbol.foldCase = Symbol.NO_FOLD_CASE;
					} else if(s.equals("--")) {
						if(end)  System.exit(0);
						return opt;
					} else if(s.equals("-")) {
						if(end)  System.exit(0);
						return opt - 1;
					} else if(s.equalsIgnoreCase("--r5rs")) {
						// it has been processed
					} else if(s.equals("--locale")) {
						// it has been processed
						state = LOCALE;
					} else if(s.equals("--import")) {
						// it has been processed
						state = LIB_SELECT;
//					} else if(s.equals("--import-all")) {
//						// it has been processed
					} else if(s.equals(REPORT_LOAD_TIME)) {
						endtime = System.currentTimeMillis();
						System.err.print("Load time: ");
						System.err.print(
								(endtime - loadTime) / 1000.0);
						System.err.println(" seconds");
					} else if(s.equals("--load-procedure-eager")) {
						// it has been processed
					} else {
						System.err.print("Invalid option: ");
						System.err.println(s);
						System.exit(2);
					}
					break;
				case EVAL:
					eval.readFile(new StringReader(s));
					end = true;
					state = INIT;
					break;
				case EXEC:
					System.out.println(LispUtils.print(eval.exec(s)));
					end = true;
					state = INIT;
					break;
				case LOCALE:  break;
				}
			}
		} catch(ReadFileException e) {
			System.err.println(e.getMessage());
			System.exit(2);
		} catch (IOException e) {
			System.err.println(e.getMessage());
			System.exit(2);
		}

		if(end)  System.exit(0);
		return opt;
	}

}
