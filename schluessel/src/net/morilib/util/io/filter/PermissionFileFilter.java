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
package net.morilib.util.io.filter;

import java.io.File;
import java.io.FileFilter;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/22
 */
public class PermissionFileFilter implements FileFilter {

	private String relop;
	private int perms;

	public PermissionFileFilter(String relop, int p) {
		this.relop = relop;
		this.perms = p;
	}

	public PermissionFileFilter(String flg) {
		relop = "=";
		if(flg.equals("r")) {
			perms = 4;
		} else if(flg.equals("w")) {
			perms = 2;
		} else if(flg.equals("x")) {
			perms = 1;
		} else {
			throw new IllegalArgumentException();
		}
	}

	@Override
	public boolean accept(File pathname) {
		int p = 0;

		p |= (pathname.canRead() ? 1 : 0) << 2;
		p |= (pathname.canWrite() ? 1 : 0) << 1;
		p |= pathname.canExecute() ? 1 : 0;
		if(relop.equals("<")) {
			return (p & perms) != 0 && p != perms;
		} else if(relop.equals("<=")) {
			return (p & perms) != 0;
		} else if(relop.equals(">")) {
			return (perms & p) != 0 && p != perms;
		} else if(relop.equals(">=")) {
			return (perms & p) != 0;
		} else if(relop.equals("=")) {
			return perms == p;
		} else if(relop.equals("!=")) {
			return perms != p;
		} else {
			throw new RuntimeException();
		}
	}

	public String toString() {
		return "perm" + relop + Integer.toOctalString(perms);
	}

}
