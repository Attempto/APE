// This file is part of the Attempto Parsing Engine (APE).
// Copyright 2008-2012, Attempto Group, University of Zurich (see http://attempto.ifi.uzh.ch).
//
// The Attempto Parsing Engine (APE) is free software: you can redistribute it and/or modify it
// under the terms of the GNU Lesser General Public License as published by the Free Software
// Foundation, either version 3 of the License, or (at your option) any later version.
//
// The Attempto Parsing Engine (APE) is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
// PURPOSE. See the GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License along with the Attempto
// Parsing Engine (APE). If not, see http://www.gnu.org/licenses/.

package ch.uzh.ifi.attempto.ape;

/**
 * This class contains ACE-related utility methods.
 * 
 * @author Tobias Kuhn
 */
public class ACEUtils {

	private ACEUtils() {}  // no instances allowed

	/**
	 * This method can be used to determine whether an indefinite article has the form "a" or
	 * "an" if it is followed by the given word. Heuristics are used and the result might not
	 * be correct in 100% of the cases.
	 * 
	 * @param word The word that immediately follows the indefinite article.
	 * @return true if "an" should be used; false for "a".
	 */
	public static boolean useIndefiniteArticleAn(String word) {
		if (word == null || word.equals("")) return false;
		boolean an = false;
		word = word.toLowerCase();
		if (word.matches("[aeiou].*")) an = true;
		if (word.matches("[fhlmnrsx]")) an = true;
		if (word.matches("[fhlmnrsx]-.*")) an = true;
		if (word.equals("u")) an = false;
		if (word.matches("u-.*")) an = false;
		if (word.matches("u[rtn]i.*")) an = false;
		if (word.matches("use.*")) an = false;
		if (word.matches("uk.*")) an = false;
		return an;
	}

}