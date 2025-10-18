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
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/09
 */
public class TimeMonotonicToTimeTAI extends TimeUnaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.datetime.TimeUnaryArgs#execute(net.morilib.lisp.datetime.LispTime, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(
			LispTime c1a, Environment env, LispMessage mesg) {
		return c1a.toTAITime();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.datetime.TimeUnaryArgs#isValidType(net.morilib.lisp.datetime.LispTime)
	 */
	@Override
	protected boolean isValidType(LispTime time) {
		return time.getTimeType().equals(
				LispTime.TimeType.TIME_MONOTONIC);
	}

}
