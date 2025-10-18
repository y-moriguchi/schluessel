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
package net.morilib.lisp.r6rs.record;

import java.util.Set;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Procedure;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/11
 */
public interface RecordTypeDescriptor {

	/**
	 * 
	 * @return
	 */
	public String getId();

	/**
	 * 
	 * @return
	 */
	public RecordTypeDescriptor getParent();

	/**
	 * 
	 * @return
	 */
	public String getUid();

	/**
	 * 
	 * @return
	 */
	public boolean isSealed();

	/**
	 * 
	 * @return
	 */
	public boolean isOpaque();

	/**
	 * 
	 * @return
	 */
	public Set<String> getFieldNames();

	/**
	 * 
	 * @return
	 */
	public Set<String> getAllFieldNames();

	/**
	 * 
	 * @return
	 */
	public Set<String> getInheritedFieldNames();

	/**
	 * 
	 * @param name
	 * @return
	 */
	public boolean isMutable(String name);

	/**
	 * 
	 * @param rtd
	 * @param parent
	 * @param protocol
	 * @return
	 * @throws RCDCreationException
	 */
	public RecordConstructorDescriptor getRcd(
			RecordTypeDescriptor rtd,
			RecordConstructorDescriptor parent,
			Procedure protocol) throws RCDCreationException;

	/**
	 * 
	 * @param rtd
	 * @param data
	 * @throws R6RSRecordInitilizeException 
	 */
	public void initialize(R6RSRecord rtd,
			Datum... data) throws R6RSRecordInitilizeException;

	/**
	 * 
	 * @param r
	 * @return
	 */
	public boolean isInstance(R6RSRecord r);

	/**
	 * @param string
	 * @return
	 */
	public boolean hasField(String string);

}
