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
package net.morilib.lisp.swing.tree;

import javax.swing.tree.DefaultMutableTreeNode;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/14
 */
public class MyMutableTreeNode extends DefaultMutableTreeNode {

	/**
	 * 
	 */
	public MyMutableTreeNode() {
		super();
	}

	/**
	 * @param userObject
	 * @param allowsChildren
	 */
	public MyMutableTreeNode(Object userObject,
			boolean allowsChildren) {
		super(userObject, allowsChildren);
	}

	/**
	 * @param userObject
	 */
	public MyMutableTreeNode(Object userObject) {
		super(userObject);
	}

	/* (non-Javadoc)
	 * @see javax.swing.tree.DefaultMutableTreeNode#toString()
	 */
	public String toString() {
		if(userObject instanceof Datum) {
			return LispUtils.print((Datum)userObject);
		} else {
			return super.toString();
		}
	}

}
