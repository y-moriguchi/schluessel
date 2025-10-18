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
package net.morilib.lang;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/02
 */
public final class ClassLoaders {

	//
	private ClassLoaders() {}

	/**
	 * 
	 * @param url
	 * @param name
	 * @return
	 * @throws MalformedURLException
	 * @throws IOException
	 */
	public Class<?> loadClass(String url,
			String name) throws MalformedURLException, IOException {
		File file = new File(url);
		URLClassLoader loader; 
		URL urlo;

		try {
			urlo = file.getCanonicalFile().toURI().toURL();
			loader = new URLClassLoader(new URL[] { urlo });
			return loader.loadClass(name);
		} catch (ClassNotFoundException e) {
			return null;
		}
	}

	/**
	 * 
	 * @param url
	 * @param name
	 * @return
	 */
	public boolean existClass(String url, String name) {
		try {
			return loadClass(url, name) != null;
		} catch (MalformedURLException e) {
			return false;
		} catch (IOException e) {
			return false;
		}
	}

}
