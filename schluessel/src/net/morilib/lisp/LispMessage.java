/*
 * Copyright 2009 Yuichiro Moriguchi
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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import net.morilib.lisp.condition.LispCompoundCondition;
import net.morilib.lisp.condition.LispCondition;
import net.morilib.lisp.condition.LispSimpleCondition;
import net.morilib.util.Maps;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class LispMessage {

	//
	private static final String CLSNAME =
		"net/morilib/lisp/init/messages";

	//
	private static Map<Locale, LispMessage> instances =
		new HashMap<Locale, LispMessage>();
	private static List<String> pluggedName = new ArrayList<String>();
	private static List<ClassLoader> pluggedLoader =
		new ArrayList<ClassLoader>();

	/**
	 * 
	 */
	public static LispMessage ROOT_MESSAGE = getInstance(Locale.US);

	//
	private ResourceBundle messages;
	private List<ResourceBundle> pluggable =
		new ArrayList<ResourceBundle>();
	private Locale locale;

	//
	private LispMessage(Locale lc) {
		messages = ResourceBundle.getBundle(CLSNAME, lc);
		locale   = lc;
	}

	/**
	 * 
	 * @param lc
	 * @return
	 */
	public static LispMessage getInstance(Locale locale) {
		LispMessage res;
		Locale lc = (locale == null) ? Locale.getDefault() : locale;

		res = instances.get(lc);
		if(res == null) {
			synchronized(LispMessage.class) {
				res = new LispMessage(lc);
				instances.put(lc, res);
			}
		}
		res.loadplugin();
		return res;
	}

	/**
	 * 
	 * @return
	 */
	public static LispMessage getInstance() {
		return getInstance(Locale.getDefault());
	}

	/**
	 * 
	 * @param name
	 * @param loader
	 */
	public static void addPlugin(String name, ClassLoader loader) {
		synchronized(pluggedName) {
			pluggedName.add(name);
			pluggedLoader.add(loader);
		}
	}

	//
	private void loadplugin() {
		if(pluggable.size() < pluggedName.size()) {
			synchronized(pluggedName) {
				int i = pluggable.size();

				for(; i < pluggedName.size(); i++) {
					pluggable.add(ResourceBundle.getBundle(
							pluggedName.get(i), messages.getLocale(),
							pluggedLoader.get(i)));
				}
			}
		}
	}

	//
	private String get(ResourceBundle rb, String prop) {
		try {
			return messages.getString(prop);
		} catch(MissingResourceException e) {
			return ROOT_MESSAGE.messages.getString(prop);
		}
	}

	/**
	 * 
	 * @param prop
	 * @return
	 */
	public String get(String prop) {
		loadplugin();
		try {
			return get(messages, prop);
		} catch(MissingResourceException e) {
			for(ResourceBundle rb : pluggable) {
				try {
					return get(rb, prop);
				} catch(MissingResourceException e2) {
					// ignore
				}
			}
			return (messages.getString("err.unknown") +
					"(" + e.getKey() + ")");
		}
	}

	/**
	 * 
	 * @param prop
	 * @param msg2
	 * @return
	 */
	public String get(String prop, String msg2) {
		return get(prop) + ":" + msg2;
	}

	/**
	 * 
	 * @param prop
	 * @param d
	 * @return
	 */
	public String get(String prop, Datum d) {
		return get(prop) + ":" + LispUtils.getResult(d);
	}

	/**
	 * 
	 * @param prop
	 * @return
	 */
	public static LispCondition getConditionByKey(String prop) {
		LispSimpleCondition err;

		if(Maps.matchPropertyKey(
				"err.require.**", prop) != null) {
			err = LispSimpleCondition.newInstance("&assertion");
		} else if(Maps.matchPropertyKey(
				"err.*.require.**", prop) != null) {
			err = LispSimpleCondition.newInstance("&assertion");
		} else if(prop.equals("err.argument")) {
			err = LispSimpleCondition.newInstance("&assertion");
		} else if(Maps.matchPropertyKey(
				"err.read.**", prop) != null) {
			err = LispSimpleCondition.newInstance("&lexical");
		} else if(Maps.matchPropertyKey(
				"err.io.**", prop) != null) {
			err = LispSimpleCondition.newInstance("&error");
		} else if(Maps.matchPropertyKey(
				"err.jdbc.**", prop) != null) {
			err = LispSimpleCondition.newInstance("&error");
		} else if(prop.equals("err.unbound")) {
			err = LispSimpleCondition.newInstance("&undefined");
		} else if(prop.equals("err.srfi34.raised")) {
			err = LispSimpleCondition.newInstance("&non-continuable");
		} else if(prop.equals("err.user")) {
			err = LispSimpleCondition.newInstance("&non-continuable");
		} else {
			err = LispSimpleCondition.newInstance("&violation");
		}
		return err;
	}

	/**
	 * 
	 * @param prop
	 * @return
	 */
	public LispCondition getCondition(String prop) {
		LispCondition err;
		LispSimpleCondition msg;

		err = getConditionByKey(prop);
		msg = LispSimpleCondition.newInstance("&message");
		msg.setField("message", new LispString(get(prop)));
		return new LispCompoundCondition(err, msg);
	}

	/**
	 * 
	 * @param prop
	 * @return
	 */
	public LispException getError(String prop) {
		return new LispException(prop, get(prop), getCondition(prop));
	}

	/**
	 * 
	 * @param prop
	 * @param msg2
	 * @return
	 */
	public LispException getError(String prop, String msg2) {
		return new LispException(
				prop, get(prop, msg2), getCondition(prop));
	}

	/**
	 * 
	 * @param prop
	 * @param d
	 * @return
	 */
	public LispException getError(String prop, Datum d) {
		return new LispException(prop, get(prop, d),
				getCondition(prop));
	}

	/**
	 * 
	 * @param prop
	 * @param msg2
	 * @return
	 */
	public LispException getError(String prop, int msg2) {
		return new LispException(
				prop, get(prop, Integer.toString(msg2)),
				getCondition(prop));
	}

	/**
	 * 
	 * @param prop
	 * @return
	 */
	public LispException getError(String prop, Throwable th) {
		return new LispException(prop, get(prop), getCondition(prop),
				th);
	}

	/**
	 * 
	 * @param prop
	 * @param msg2
	 * @return
	 */
	public LispException getError(
			String prop, String msg2, Throwable th) {
		return new LispException(
				prop, get(prop, msg2), getCondition(prop), th);
	}

	/**
	 * 
	 * @param prop
	 * @param d
	 * @return
	 */
	public LispException getError(
			String prop, Datum d, Throwable th) {
		return new LispException(
				prop, get(prop, d), getCondition(prop), th);
	}

	/**
	 * 
	 * @param prop
	 * @return
	 */
	public ReadException getReadError(String prop) {
		return new ReadException(prop, get(prop), getCondition(prop));
	}

	/**
	 * 
	 * @param prop
	 * @param msg2
	 * @return
	 */
	public ReadException getReadError(String prop, String msg2) {
		return new ReadException(prop, get(prop, msg2),
				getCondition(prop));
	}

	/**
	 * 
	 * @param prop
	 * @param d
	 * @return
	 */
	public ReadException getReadError(String prop, Datum d) {
		return new ReadException(prop, get(prop, d),
				getCondition(prop));
	}

	/**
	 * 
	 * @param e
	 */
	public LispException getUncaughtException(LispException e) {
		return new LispException(
				"err.srfi18.uncaught",
				get("err.srfi18.uncaught"),
				getCondition("err.srfi18.uncaught"), e);
	}

	/**
	 * 
	 * @param prop
	 */
	public void warn(String prop) {
		System.err.print(get("warn.repl.err"));
		System.err.print(" ");
		System.err.println(get(prop));
	}

	/**
	 * @return the locale
	 */
	public Locale getLocale() {
		return locale;
	}

}
