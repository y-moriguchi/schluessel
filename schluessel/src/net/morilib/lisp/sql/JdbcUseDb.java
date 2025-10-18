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
package net.morilib.lisp.sql;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.List;
import java.util.Map;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Parser;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/09
 */
public class JdbcUseDb extends UnaryArgs {

	//
	private static Map<Datum, Datum> drivers;

	static {
		InputStream ins = null;
		List<Datum> l;

		try {
			ins = JdbcUseDb.class.getResourceAsStream("dbconfig.s");
			l   = Parser.readSExpression(
					new InputStreamReader(ins, "UTF-8"));
			drivers = LispUtils.assocToMap(l.get(0));
		} catch (UnsupportedEncodingException e) {
			throw new RuntimeException(e);
		} catch (IOException e) {
			throw new RuntimeException(e);
		} finally {
			if(ins != null) {
				try {
					ins.close();
				} catch (IOException e) {
					throw new RuntimeException(e);
				}
			}
		}

		for(Map.Entry<Datum, Datum> e :
				LispUtils.assocToMap(l.get(1)).entrySet()) {
			if(e.getValue().isTrue()) {
				Class<?> cl;
				Constructor<?> cn;

				try {
					cl = Class.forName(e.getValue().getString());
					cn = cl.getConstructor(String.class);
					cn.newInstance(((Symbol)e.getKey()).getName());
				} catch (ClassNotFoundException e1) {
					throw new RuntimeException(e1);
				} catch (NoSuchMethodException e1) {
					throw new RuntimeException(e1);
				} catch (IllegalArgumentException e1) {
					throw new RuntimeException(e1);
				} catch (InstantiationException e1) {
					throw new RuntimeException(e1);
				} catch (IllegalAccessException e1) {
					throw new RuntimeException(e1);
				} catch (InvocationTargetException e1) {
					throw new RuntimeException(e1);
				}
			}
		}
	}

	//
	/*package*/ void setDriver(Datum sym, Datum drv) {
		drivers.put(sym, drv);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(
			Datum c1a, Environment env, LispMessage mesg) {
		if(c1a instanceof Symbol) {
			Datum r = drivers.get(c1a);

			if(r == null) {
				throw mesg.getError("err.jdbc.dbnotfound", c1a);
			} else {
				if(r.isTrue()) {
					try {
						Class.forName(r.getString());
					} catch (ClassNotFoundException e) {
						throw mesg.getError(
								"err.jdbc.drivernotfound", c1a);
					}
				}
				LispSQLUtils.setSQLUtils(((Symbol)c1a).getName());
				return Undef.UNDEF;
			}
		} else {
			throw mesg.getError("err.require.symbol", c1a);
		}
	}

}
