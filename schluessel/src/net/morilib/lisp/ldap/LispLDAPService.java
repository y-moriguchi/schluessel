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
package net.morilib.lisp.ldap;

import java.util.ArrayList;
import java.util.List;

import javax.naming.directory.Attribute;
import javax.naming.directory.Attributes;
import javax.naming.directory.BasicAttribute;
import javax.naming.directory.BasicAttributes;
import javax.naming.directory.DirContext;
import javax.naming.directory.ModificationItem;
import javax.naming.directory.SearchControls;

import net.morilib.ldap.AndAttributeFilter;
import net.morilib.ldap.AttributeFilter;
import net.morilib.ldap.FilterRelation;
import net.morilib.ldap.LDAPManager;
import net.morilib.ldap.NotAttributeFilter;
import net.morilib.ldap.OrAttributeFilter;
import net.morilib.ldap.SimpleAttributeFilter;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispException;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/14
 */
public class LispLDAPService extends Datum2 {

	//
	private static final Symbol AND = Symbol.getSymbol("and");
	private static final Symbol OR  = Symbol.getSymbol("or");
	private static final Symbol NOT = Symbol.getSymbol("not");
	private static final Symbol EQ  = Symbol.getSymbol("=");
	private static final Symbol AEQ = Symbol.getSymbol("~=");
	private static final Symbol LE  = Symbol.getSymbol("<=");
	private static final Symbol GE  = Symbol.getSymbol(">=");
	private static final Symbol OBJECT   = Symbol.getSymbol("object");
	private static final Symbol ONELEVEL = Symbol.getSymbol("1level");
	private static final Symbol SUBTREE  = Symbol.getSymbol("subtree");
	private static final Symbol ADD     = Symbol.getSymbol("add");
	private static final Symbol REPLACE = Symbol.getSymbol("replace");
	private static final Symbol REMOVE  = Symbol.getSymbol("remove");

	//
	LDAPManager manager;

	/**
	 * 
	 * @param manager
	 */
	public LispLDAPService(LDAPManager manager) {
		this.manager = manager;
	}

	/**
	 * 
	 * @param d
	 * @param mesg
	 * @return
	 */
	public static AttributeFilter parseFilter(Datum d,
			LispMessage mesg) {
		try {
			AttributeFilter[] af;
			List<Datum> l = LispUtils.consToList(d, mesg);

			if(l.get(0).equals(AND)) {
				af = new AttributeFilter[l.size() - 1];
				for(int i = 0; i < af.length; i++) {
					af[i] = parseFilter(l.get(i + 1), mesg);
				}
				return new AndAttributeFilter(af);
			} else if(l.get(0).equals(OR)) {
				af = new AttributeFilter[l.size() - 1];
				for(int i = 0; i < af.length; i++) {
					af[i] = parseFilter(l.get(i + 1), mesg);
				}
				return new OrAttributeFilter(af);
			} else if(l.get(0).equals(NOT) && l.size() == 2) {
				return new NotAttributeFilter(
						parseFilter(l.get(1), mesg));
			} else if(l.get(0).equals(EQ) && l.size() == 3) {
				return new SimpleAttributeFilter(
						SubrUtils.getString(l.get(1), mesg),
						SubrUtils.getString(l.get(2), mesg),
						FilterRelation.EQUAL);
			} else if(l.get(0).equals(AEQ) && l.size() == 3) {
				return new SimpleAttributeFilter(
						SubrUtils.getString(l.get(1), mesg),
						SubrUtils.getString(l.get(2), mesg),
						FilterRelation.APPROX);
			} else if(l.get(0).equals(LE) && l.size() == 3) {
				return new SimpleAttributeFilter(
						SubrUtils.getString(l.get(1), mesg),
						SubrUtils.getString(l.get(2), mesg),
						FilterRelation.LESS);
			} else if(l.get(0).equals(GE) && l.size() == 3) {
				return new SimpleAttributeFilter(
						SubrUtils.getString(l.get(1), mesg),
						SubrUtils.getString(l.get(2), mesg),
						FilterRelation.GREATER);
			} else {
				throw mesg.getError("err.ldap.filter.invalid", d);
			}
		} catch(LispException e) {
			throw mesg.getError("err.ldap.filter.invalid", d);
		}
	}

	/**
	 * 
	 * @param d
	 * @param mesg
	 * @return
	 */
	public static int parseScope(Datum d, LispMessage mesg) {
		if(d.equals(OBJECT)) {
			return SearchControls.OBJECT_SCOPE;
		} else if(d.equals(ONELEVEL)) {
			return SearchControls.ONELEVEL_SCOPE;
		} else if(d.equals(SUBTREE)) {
			return SearchControls.SUBTREE_SCOPE;
		} else {
			throw mesg.getError("err.ldap.scope.invalid", d);
		}
	}

	/**
	 * 
	 * @param d
	 * @param mesg
	 * @return
	 */
	public static Attributes parseAttributes(Datum d,
			LispMessage mesg) {
		ConsIterator itr, jtr;
		Attributes as = new BasicAttributes();
		Attribute  at;
		String s1;

		try {
			itr = new ConsIterator(d);
			while(itr.hasNext()) {
				jtr = new ConsIterator(itr.next());
				s1  = SubrUtils.nextString(jtr, mesg, d);
				at  = new BasicAttribute(s1);
				while(jtr.hasNext()) {
					at.add(SubrUtils.nextString(jtr, mesg, d));
				}
				SubrUtils.checkProper(jtr, d, mesg);
				as.put(at);
			}
			SubrUtils.checkProper(itr, d, mesg);
		} catch(LispException e) {
			throw mesg.getError("err.ldap.attrs.invalid", d);
		}
		return as;
	}

	/**
	 * 
	 * @param d
	 * @param mesg
	 * @return
	 */
	public static ModificationItem[] parseModification(Datum d,
			LispMessage mesg) {
		ConsIterator itr, jtr;
		List<ModificationItem> ms = new ArrayList<ModificationItem>();
		ModificationItem mi;
		Attribute  at;
		String s1;
		Datum k1;
		int mf;

		try {
			itr = new ConsIterator(d);
			while(itr.hasNext()) {
				jtr = new ConsIterator(itr.next());
				k1  = SubrUtils.nextIf(jtr, mesg, d);
				if(k1.equals(ADD)) {
					mf = DirContext.ADD_ATTRIBUTE;
				} else if(k1.equals(REPLACE)) {
					mf = DirContext.REPLACE_ATTRIBUTE;
				} else if(k1.equals(REMOVE)) {
					mf = DirContext.REMOVE_ATTRIBUTE;
				} else {
					throw mesg.getError("err.ldap.attrs.invalid", d);
				}

				s1  = SubrUtils.nextString(jtr, mesg, d);
				at  = new BasicAttribute(s1);
				while(jtr.hasNext()) {
					at.add(SubrUtils.nextString(jtr, mesg, d));
				}
				SubrUtils.checkProper(jtr, d, mesg);
				mi  = new ModificationItem(mf, at);
				ms.add(mi);
			}
			SubrUtils.checkProper(itr, d, mesg);
		} catch(LispException e) {
			throw mesg.getError("err.ldap.attrs.invalid", d);
		}
		return ms.toArray(new ModificationItem[0]);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<ldap-service>");
	}

}
