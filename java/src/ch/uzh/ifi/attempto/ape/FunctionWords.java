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

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

/**
 * This class contains the function words of ACE. These words have a predefined meaning and cannot
 * be used in the lexicon. Function words like "can" or "true", which can be used in the lexicon,
 * are not covered by this class.
 * 
 * @author Tobias Kuhn
 */
public class FunctionWords {
	
	private static HashSet<String> functionWords = new HashSet<String>(Arrays.asList(new String[] {
			"null", "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven",
			"twelve", "dozen", "there", "and", "or", "not", "that", "than", "of", "if", "then", "such", "be", "provably",
			"more", "most", "are", "is", "the", "a", "an", "some", "no", "every", "all", "each", "which", "its", "his",
			"her", "their", "whose", "it", "he", "she", "they", "him", "them", "itself", "himself", "herself",
			"themselves", "someone", "somebody", "something", "nobody", "nothing", "everyone", "everybody", "everything",
			"what", "who", "how", "where", "when", "by", "Null", "Zero", "One", "Two", "Three", "Four", "Five", "Six",
			"Seven", "Eight", "Nine", "Ten", "Eleven", "Twelve", "Dozen", "There", "If", "Are", "Is", "The", "A", "An",
			"Some", "No", "Every", "All", "Each", "Which", "Its", "His", "Her", "Their", "Whose", "It", "He", "She",
			"They", "Someone", "Somebody", "Something", "Nobody", "Nothing", "Everyone", "Everybody", "Everything",
			"What", "Who", "How", "Where", "When"
		}));
	
	private FunctionWords() {}  // no instances allowed
	
	/**
	 * Checks whether a certain string represents a function word.
	 * 
	 * @param s The string to be checked.
	 * @return true if the string represents a function word.
	 */
	public static boolean isFunctionWord(String s) {
		return functionWords.contains(s);
	}
	
	/**
	 * Returns a set that contains all function words.
	 * 
	 * @return A list containing all function words.
	 */
	public static Set<String> getFunctionWords() {
		return new HashSet<String>(functionWords);
	}

}
