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
package net.morilib.options;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/15
 */
public final class OptionUtils {

	//
	private OptionUtils() {}

	//
	private static Object checkreq(boolean req) {
//		if(req) {
//			throw new IllegalCommandLineException(
//					"argument required");
//		}
//		return null;
		return req ? Boolean.FALSE : null;
	}

	//
	private static Object checkopt(boolean req, boolean opt,
			ArgsIterator args) {
		if(!req && !opt) {
//			throw new IllegalCommandLineException(
//					"the option can not have any arguments");
			return Boolean.FALSE;
		}
		return args.hasNext() ? args.next() : null;
	}

	//
	static Object processLong(int optchar, String s,
			boolean req, boolean opt, ArgsIterator args) {
		String a;
		Object o;
		int x;

		if(!args.isLongOption(optchar)) {
			return Boolean.FALSE;
		} else if((a = args.peek()).indexOf(s) != 2) {
			return Boolean.FALSE;
		} else if((x = s.length() + 2) < a.length()) {
			if(a.charAt(x) != '=') {
				return Boolean.FALSE;
			} else if(checkopt(req, opt, args) == Boolean.FALSE) {
				args.forward();
				return null;
			} else {
				args.forward();
				return a.substring(++x);
			}
		} else if(args.forward().isDelim(optchar)) {
			return checkreq(req);
		} else if((o = checkopt(req, opt, args)) == Boolean.FALSE) {
			return null;
		} else {
			return o;
		}
	}

	//
	static Object process(int optchar, char c,
			boolean req, boolean opt, ArgsIterator args) {
		String a;
		Object o;
		int x;

		if(args.isShortOption(optchar) &&
				(a = args.peek()).indexOf(c) == 1) {
			if(req || opt) {
				if(a.length() > 2) {
					args.forward();
					return a.substring(2);
				} else if(!args.forward().isDelim(optchar)) {
					return checkopt(req, opt, args);
				} else if((o = checkreq(req)) == Boolean.FALSE) {
					throw new IllegalCommandLineException(
							"the option can not have any arguments");
				} else {
					return o;
				}
			} else if(args.forwardShort().isDelim(optchar)) {
				return checkreq(req);
//			} else if(!opt) {
//				throw new IllegalCommandLineException(
//						"the option can not have any arguments");
			} else {
				return null;
			}
		} else if(args.isLongOption(optchar) &&
				(x = (a = args.peek()).indexOf(c)) == 2) {
			if(a.length() > 3) {
				if(checkopt(req, opt, args) == Boolean.FALSE) {
					return Boolean.FALSE;
				}
				if(a.charAt(x + 1) == '=')  x++;
				return a.substring(x + 1);
			} else if(args.forward().isDelim(optchar)) {
				return checkreq(req);
			} else {
				return checkopt(req, opt, args);
			}
		} else {
			return Boolean.FALSE;
		}
	}

	//
	static Object process(int oc, String s,
			boolean req, boolean opt, ArgsIterator args) {
		switch(s.length()) {
		case 0:   throw new IllegalArgumentException();
		case 1:   return process(oc, s.charAt(0), req, opt, args);
		default:  return processLong(oc, s, req, opt, args);
		}
	}

	/**
	 * 
	 * @param optchar
	 * @param args
	 * @param options
	 * @param unrecognized
	 * @param operand
	 * @param extra
	 * @return
	 */
	public static Object process(int optchar, String[] args,
			OptionObject[] options,
			OptionProcessor unrecognized,
			OperandProcessor operand,
			Object extra) {
		ArgsIterator a2 = new ArgsIterator(args);
		Object x, e2 = extra;

		outer: while(a2.isOption(optchar)) {
			for(OptionObject o : options) {
				for(String s : o.getNames()) {
					x = process(optchar, s,
							o.isArgumentRequired(),
							o.isArgumentOptional(),
							a2);
					if(x != Boolean.FALSE) {
						e2 = o.getProcessor().call(
								o, s, (String)x, e2);
						continue outer;
					}
				}
			}
			e2 = unrecognized.call(null,
					a2.next().replaceFirst("^-+", ""), null, extra);
		}

		for(String s : a2.operands())  e2 = operand.call(s, e2);
		return e2;
	}

	/**
	 * 
	 * @param args
	 * @param options
	 * @param unrecognized
	 * @param operand
	 * @param extra
	 * @return
	 */
	public static Object process(String[] args,
			OptionObject[] options,
			OptionProcessor unrecognized,
			OperandProcessor operand,
			Object extra) {
		return process('-',
				args, options, unrecognized, operand, extra);
	}

}
