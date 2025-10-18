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
package net.morilib.lisp.locale;

import java.util.Locale;

import net.morilib.lisp.Datum2;
import net.morilib.lisp.JavaObjective;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/21
 */
public class LispLocale extends Datum2
implements JavaObjective, java.io.Serializable, Cloneable {

	//
	Locale locale;

	/**
	 * 
	 * @param locale
	 */
	public LispLocale(Locale locale) {
		if(locale == null)  throw new NullPointerException();
		this.locale = locale;
	}

	/**
	 * @return the locale
	 */
	public Locale getLocale() {
		return locale;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.JavaObjective#toObject()
	 */
	@Override
	public Object toObject() {
		return locale;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return locale.hashCode();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof LispLocale) {
			return locale.equals(((LispLocale)o).locale);
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#clone()
	 */
	@Override
	public Object clone() {
		return new LispLocale((Locale)locale.clone());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<locale ").append(locale).append(">");
	}

}
