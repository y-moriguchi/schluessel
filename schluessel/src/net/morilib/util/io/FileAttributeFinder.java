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
package net.morilib.util.io;

import java.io.FileFilter;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import net.morilib.util.io.filter.AndFileFilter;
import net.morilib.util.io.filter.HiddenFileFilter;
import net.morilib.util.io.filter.MtimeFileFilter;
import net.morilib.util.io.filter.NotFileFilter;
import net.morilib.util.io.filter.OrFileFilter;
import net.morilib.util.io.filter.PermissionFileFilter;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/22
 */
public final class FileAttributeFinder {

	static final SimpleDateFormat FORMAT;
	static final int INIT = 1000;
	static final int STAT_EQ = 1010;
	static final int STAT_LT = 1020;
	static final int STAT_GT = 1030;
	static final int STAT_STR = 1040;
	static final int STAT_NE = 1050;

	static {
		FORMAT = new SimpleDateFormat("yyyy-MM-dd");
		FORMAT.setLenient(false);
	}

	//
	static List<String> lexer(String s) {
		StringBuffer b = new StringBuffer();
		List<String> r = new ArrayList<String>();
		int stat = INIT, c, i = 0;

		while(true) {
			c = i < s.length() ? s.charAt(i++) : -1;
			switch(stat) {
			case INIT:
				switch(c) {
				case -1:  return r;
				case '=':  stat = STAT_EQ;  break;
				case '<':  stat = STAT_LT;  break;
				case '>':  stat = STAT_GT;  break;
				case '(':  r.add("(");  break;
				case ')':  r.add(")");  break;
				case '&':  r.add("&");  break;
				case '|':  r.add("|");  break;
				case '!':  stat = STAT_NE;  break;
				default:
					b.append((char)c);  stat = STAT_STR;  break;
				}
				break;
			case STAT_EQ:
				switch(c) {
				case -1:  case '=':
					r.add("=");  break;
				default:  i--;  r.add("=");  break;
				}
				stat = INIT;
				break;
			case STAT_LT:
				switch(c) {
				case -1:   r.add("<");  break;
				case '=':  r.add("<=");  break;
				case '>':  r.add("!=");  break;
				default:   r.add("<");  i--;  break;
				}
				stat = INIT;
				break;
			case STAT_GT:
				switch(c) {
				case -1:   r.add(">");  break;
				case '=':  r.add(">=");  break;
				default:   r.add(">");  i--;  break;
				}
				stat = INIT;
				break;
			case STAT_STR:
				switch(c) {
				case -1:
					r.add(b.toString());  stat = INIT;
					break;
				case '=':  case '<':  case '>':
				case '(':  case ')':
				case '&':  case '|':  case '!':
					i--;  r.add(b.toString());
					b = new StringBuffer();
					stat = INIT;
					break;
				default:  b.append((char)c);  break;
				}
				break;
			case STAT_NE:
				switch(c) {
				case -1:   r.add("!");  break;
				case '=':  r.add("!=");  break;
				default:   r.add("!");  i--;  break;
				}
				stat = INIT;
				break;
			}
		}
	}

	//
	static String nxt(Iterator<String> tok) {
		String s;

		s = tok.hasNext() ? tok.next() : null;
		return s;
	}

	//
	static int octalperm(String s) {
		int r;

		try {
			r = Integer.parseInt(s, 8);
			return (r < 0 || r > 07) ? -1 : r;
		} catch(NumberFormatException e) {
			return -1;
		}
	}

	//
	static FileFilter perm(Iterator<String> tok) {
		String s = nxt(tok), t;
		int p;

		if(s.equals("=")) {
			t = nxt(tok);
			if((p = octalperm(t)) < 0) {
				try {
					return new PermissionFileFilter(t);
				} catch(IllegalArgumentException e) {
					throw new FileAttributeFinderSyntaxException();
				}
			} else {
				return new PermissionFileFilter(s, p);
			}
		} else if(s.equals("<") || s.equals("<=") ||
				s.equals(">") || s.equals(">=") || s.equals("!=")) {
			t = nxt(tok);
			if((p = octalperm(t)) < 0) {
				throw new FileAttributeFinderSyntaxException();
			}
			return new PermissionFileFilter(s, p);
		} else {
			throw new FileAttributeFinderSyntaxException();
		}
	}

	//
	static FileFilter mtime(Iterator<String> tok) {
		String s = nxt(tok), t;
		java.util.Date d;

		if(s.equals("=") || s.equals("<") || s.equals("<=") ||
				s.equals(">") || s.equals(">=") || s.equals("!=")) {
			if(!(t = nxt(tok)).matches(
					"^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$")) {
				throw new FileAttributeFinderSyntaxException(t);
			}

			try {
				d = FORMAT.parse(t);
				return new MtimeFileFilter(s, d);
			} catch(ParseException e) {
				throw new FileAttributeFinderSyntaxException(e);
			}
		} else {
			throw new FileAttributeFinderSyntaxException();
		}
	}

	//
	static FileFilter getAnd(List<FileFilter> l) {
		return (l.size() == 1) ? l.get(0) : new AndFileFilter(l);
	}

	//
	static FileFilter getOr(List<FileFilter> l) {
		return (l.size() == 1) ? l.get(0) : new OrFileFilter(l);
	}

	//
	static boolean isend(String s, String e) {
		return ((s == null && e == null) ||
				(s != null && s.equals(e)));
	}

	//
	static boolean ismismatch(String s, String e) {
		return ((s != null && !s.equals(e)) ||
				(s == null && e != null));
	}

	//
	static FileFilter start2(String s, String e,
			Iterator<String> tok) {
		List<FileFilter> l = new ArrayList<FileFilter>();
		FileFilter s1;
		Object[] o;

		s1 = start4(s, e, tok);
		while(true) {
			s = nxt(tok);
			if(isend(s, e)) {
				l.add(s1);
				return getOr(l);
			} else if(s != null && s.equals("&")) {
				o = start3(s1, s, e, tok);
				l.add((FileFilter)o[0]);
				s = (String)o[1];
				if(s != null && s.equals("|")) {
					s  = nxt(tok);
					s1 = start4(s, e, tok);
				} else if(isend(s, e)) {
					return getOr(l);
				} else if(ismismatch(s, e)) {
					throw new FileAttributeFinderSyntaxException(s);
				} else {
					s1 = start4(s, e, tok);
				}
			} else if(s != null && s.equals("|")) {
				l.add(s1);
				s  = nxt(tok);
				s1 = start4(s, e, tok);
			} else {
				throw new FileAttributeFinderSyntaxException(s);
			}
		}
	}

	//
	static Object[] start3(FileFilter s1, String s, String e,
			Iterator<String> tok) {
		List<FileFilter> l = new ArrayList<FileFilter>();

		l.add(s1);
		while(true) {
			if(s == null && e == null) {
				return new Object[] { getAnd(l), null };
			} else if(s != null && s.equals(e)) {
				return new Object[] { getAnd(l), s };
			} else if(s != null && s.equals("|")) {
				return new Object[] { getAnd(l), s };
			} else if(s == null || !s.equals("&")) {
				throw new FileAttributeFinderSyntaxException(s);
			}
			l.add(start4(nxt(tok), e, tok));
			s = nxt(tok);
		}
	}

	//
	static FileFilter start4(String s, String e,
			Iterator<String> tok) {
		if(s == null) {
			throw new FileAttributeFinderSyntaxException("null");
		} else if(s.equals("(")) {
			return start2(nxt(tok), ")", tok);
		} else if(s.equals("!")) {
			return new NotFileFilter(start4(nxt(tok), e, tok));
		} else if(s.matches("^p(erm(ission)?)?$")) {
			return perm(tok);
		} else if(s.matches("^m((odified-)?time)?$")) {
			return mtime(tok);
		} else if(s.matches("^h(idden)?$")) {
			return HiddenFileFilter.INSTANCE;
		} else {
			throw new FileAttributeFinderSyntaxException(s);
		}
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static FileFilter compile(String s) {
		Iterator<String> tok;

		if(s == null) {
			throw new NullPointerException();
		} else if(s.equals("")) {
			throw new FileAttributeFinderSyntaxException();
		} else {
			tok = lexer(s.replaceAll("[ \t]+", "")).iterator();
			return start2(nxt(tok), null, tok);
		}
	}

}
