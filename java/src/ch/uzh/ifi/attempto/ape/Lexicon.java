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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * This class represents a lexicon which consists of a set of lexicon entries.
 * 
 * @author Tobias Kuhn
 */
public class Lexicon {

	private List<LexiconEntry> entries = new ArrayList<LexiconEntry>();


	/**
	 * Creates an empty lexicon.
	 */
	public Lexicon() {
	}

	/**
	 * Adds a lexicon entry to this lexicon.
	 * 
	 * @param entry The lexicon entry to be added.
	 */
	public void addEntry(LexiconEntry entry) {
		if (!entries.contains(entry)) {
			entries.add(entry);
		}
	}

	/**
	 * Adds a collection of lexicon entries to this lexicon.
	 * 
	 * @param entries The lexicon entries to be added.
	 */
	public void addEntries(Collection<LexiconEntry> entries) {
		for (LexiconEntry entry : entries) {
			addEntry(entry);
		}
	}

	/**
	 * Removes the lexicon entry from this lexicon.
	 * 
	 * @param entry The lexicon entry to be removed.
	 */
	public void removeEntry(LexiconEntry entry) {
		entries.remove(entry);
	}

	/**
	 * Removes all entries from this lexicon.
	 */
	public void removeAllEntries() {
		entries.clear();
	}

	/**
	 * Returns a list of all lexicon entries that are contained in this lexicon.
	 * 
	 * @return A list of all lexicon entries.
	 */
	public List<LexiconEntry> getEntries() {
		return new ArrayList<LexiconEntry>(entries);
	}
	
	/**
	 * Returns the lexicon as a serialized Prolog list.
	 * 
	 * @return The lexicon as a serialized Prolog list.
	 */
	public String toList() {
		StringBuilder sb = new StringBuilder();
		for (LexiconEntry entry : entries) {
			sb.append(entry.toString());
			sb.append(", ");
		}
		if (sb.length() > 0) {
			sb.substring(0, sb.length()-2);
		}
		return "[" + sb.toString() + "]";
	}

	/**
	 * Returns this lexicon as a contents of an APE lexicon file, i.e.
	 * each entry is followed by a dot ('.') and a space (' ').
	 */
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		for (LexiconEntry entry : entries) {
			sb.append(entry.toString());
			sb.append(". ");
		}
		return sb.toString();
	}
	
}