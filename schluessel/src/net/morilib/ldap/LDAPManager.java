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
package net.morilib.ldap;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import javax.naming.Context;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.directory.Attribute;
import javax.naming.directory.Attributes;
import javax.naming.directory.InitialDirContext;
import javax.naming.directory.ModificationItem;
import javax.naming.directory.SearchControls;
import javax.naming.directory.SearchResult;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/14
 */
public final class LDAPManager {

	//
	private InitialDirContext dir;

	/**
	 * 
	 * @param hostname
	 * @param rootdn
	 * @param rootpw
	 * @throws NamingException
	 */
	public LDAPManager(String hostname,
			String rootdn,
			String rootpw) throws NamingException {
		Hashtable<Object, Object> env =
			new Hashtable<Object, Object>();

		env.put(Context.INITIAL_CONTEXT_FACTORY,
				"com.sun.jndi.ldap.LdapCtxFactory");
		env.put(Context.PROVIDER_URL, "ldap://" + hostname);
		env.put("java.naming.ldap.version", "3");
		env.put(Context.SECURITY_PRINCIPAL, rootdn.toString());
		env.put(Context.SECURITY_CREDENTIALS, rootpw);
		dir = new InitialDirContext(env);
	}

	/**
	 * 
	 * @param hostname
	 * @param rootdn
	 * @param rootpw
	 * @throws NamingException
	 */
	public LDAPManager(String hostname,
			DistinguishedName rootdn,
			String rootpw) throws NamingException {
		this(hostname, rootdn.toString(), rootpw);
	}

	/**
	 * 
	 * @param dn
	 * @param attrs
	 * @throws NamingException
	 */
	public void add(String dn,
			Attributes attrs) throws NamingException {
		dir.createSubcontext(dn, attrs);
	}

	/**
	 * 
	 * @param basedn
	 * @param filter
	 * @param searchScope
	 * @return
	 * @throws NamingException
	 */
	public List<Map<String, List<String>>> search(
			String basedn, AttributeFilter filter,
			int searchScope) throws NamingException {
		SearchControls ctr = new SearchControls();
		NamingEnumeration<SearchResult> resenum;
		List<Map<String, List<String>>> r =
			new ArrayList<Map<String, List<String>>>();

		ctr.setSearchScope(searchScope);
		resenum = dir.search(basedn,
				"(" + filter.toString() + ")",
				ctr);

		while(resenum.hasMore()) {
			SearchResult res = resenum.next();
			NamingEnumeration<? extends Attribute> attrs =
				res.getAttributes().getAll();
			Map<String, List<String>> e =
				new HashMap<String, List<String>>();

			while(attrs.hasMore()) {
				Attribute attr = attrs.nextElement();
				Enumeration<?> venum = attr.getAll();
				List<String> vl = new ArrayList<String>();

				while(venum.hasMoreElements()) {
					vl.add((String)venum.nextElement());
				}
				e.put(attr.getID(), vl);
			}
			r.add(e);
		}
		return r;
	}

	/**
	 * 
	 * @param basedn
	 * @param filter
	 * @param searchScope
	 * @return
	 * @throws NamingException
	 */
	public List<Map<String, List<String>>> search(
			DistinguishedName basedn, AttributeFilter filter,
			int searchScope) throws NamingException {
		return search(basedn.toString(), filter, searchScope);
	}

	/**
	 * 
	 * @param dn
	 * @param moditems
	 * @throws NamingException
	 */
	public void modify(String dn,
			ModificationItem[] moditems) throws NamingException {
		dir.modifyAttributes(dn.toString(), moditems);
	}

	/**
	 * 
	 * @param dn
	 * @throws NamingException
	 */
	public void delete(String dn) throws NamingException {
		dir.destroySubcontext(dn.toString());
	}

}
