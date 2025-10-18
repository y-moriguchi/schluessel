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
package net.morilib.lisp.format;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public final class MapFormatCommandFactory
implements FormatCommandFactory {
	
	
	private static Map<Integer, Class<? extends FormatCommand>> clsmap;
	private static final MapFormatCommandFactory INSTANCE =
		new MapFormatCommandFactory();
	
	static {
		clsmap = new HashMap<Integer, Class<? extends FormatCommand>>();
		setClassMap('A', FormatCommandA.class);
		setClassMap('S', FormatCommandS.class);
		setClassMap('%', FormatCommandPercent.class);
		setClassMap('~', FormatCommandTilde.class);
		setClassMap('D', FormatCommandD.class);
		setClassMap('B', FormatCommandB.class);
		setClassMap('O', FormatCommandO.class);
		setClassMap('X', FormatCommandX.class);
	}
	
	
	public static MapFormatCommandFactory getInstance() {
		return INSTANCE;
	}
	
	
	public synchronized static void setClassMap(
			int c, Class<? extends FormatCommand> kls) {
		if(kls == null) {
			throw new NullPointerException();
		}
		clsmap.put(Character.toUpperCase(c), kls);
	}
	
	
	public FormatCommand newInstance(
			int c, ArgumentTypeBuf arg) throws FormatParseException {
		try {
			Class<? extends FormatCommand> kls;
			Constructor<? extends FormatCommand> cns;
			
			kls = clsmap.get(Character.toUpperCase(c));
			if(kls == null) {
				throw new FormatParseException();
			}
			
			cns = kls.getConstructor(
					List.class, Boolean.TYPE, Boolean.TYPE);
			return cns.newInstance(
					arg.getArgumentTypeList(),
					arg.isAtmark(),
					arg.isColon());
		} catch (SecurityException e) {
			throw new RuntimeException(e);
		} catch (NoSuchMethodException e) {
			throw new RuntimeException(e);
		} catch (IllegalArgumentException e) {
			throw new RuntimeException(e);
		} catch (InstantiationException e) {
			throw new RuntimeException(e);
		} catch (IllegalAccessException e) {
			throw new RuntimeException(e);
		} catch (InvocationTargetException e) {
			throw new RuntimeException(e);
		}
	}

}
