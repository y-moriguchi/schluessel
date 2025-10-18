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
package net.morilib.lisp.datetime;

import net.morilib.lisp.Datum;
import net.morilib.util.datetime.JulianDay;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/09
 */
public class LispJulianDay extends Datum
implements java.io.Serializable {

	//
	private JulianDay jd;

	/**
	 * 
	 */
	public LispJulianDay() {
		jd = new JulianDay();
	}

	/**
	 * 
	 * @param time
	 */
	public LispJulianDay(long time) {
		jd = new JulianDay(time);
	}

	/**
	 * 
	 * @param jd
	 */
	public LispJulianDay(java.util.Date jd) {
		this(jd.getTime());
	}

	/**
	 * 
	 * @return
	 */
	public JulianDay getJulianDay() {
		return new JulianDay(jd.getTime());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<julian day ")
		.append(jd.getJulianDay().doubleValue())
		.append(">");
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return jd.hashCode();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(obj instanceof LispJulianDay) {
			return jd.equals(((LispJulianDay)obj).jd);
		}
		return false;
	}

}
