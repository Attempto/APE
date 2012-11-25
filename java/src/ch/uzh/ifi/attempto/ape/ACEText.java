package ch.uzh.ifi.attempto.ape;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * WARNING: This class is experimental and is incomplete (i.e. covers only a subset of the lexicon entries).
 *
 * This class reflects the fact that an ACE text is really a combination of
 * a string and a lexicon. The lexicon is used to interpret the content words
 * in the string: assign them word classes, features (e.g. gender) and map them
 * to logical symbols.
 *
 * We experiment with a representation where the string and the lexicon are
 * packaged into a single string. Example:
 *
 * pn_sg('John','John_PN',masc) tv_finsg('buys','buy_V2') everything that isn't a noun_sg('present','present_N',neutr) .
 *
 * or maybe:
 *
 * John|pn_sg|John_PN|masc buys|tv_finsg|buy_V2 everything that isn't a present|noun_sg|present_N|neutr .
 *
 * @author Kaarel Kaljurand
 */
public class ACEText {

	public static final Pattern PN_SG = Pattern.compile("pn_sg\\('([a-zA-Z0-9_-]+)','([a-zA-Z0-9_-]+)',([a-zA-Z0-9_-]+)\\)");
	public static final Pattern NOUN_SG = Pattern.compile("noun_sg\\('([a-zA-Z0-9_-]+)','([a-zA-Z0-9_-]+)',([a-zA-Z0-9_-]+)\\)");
	public static final Pattern NOUN_PL = Pattern.compile("noun_pl\\('([a-zA-Z0-9_-]+)','([a-zA-Z0-9_-]+)',([a-zA-Z0-9_-]+)\\)");
	public static final Pattern TV_FINSG = Pattern.compile("tv_finsg\\('([a-zA-Z0-9_-]+)','([a-zA-Z0-9_-]+)'\\)");
	public static final Pattern TV_INFPL = Pattern.compile("tv_infpl\\('([a-zA-Z0-9_-]+)','([a-zA-Z0-9_-]+)'\\)");
	public static final Pattern TV_PP = Pattern.compile("tv_pp\\('([a-zA-Z0-9_-]+)','([a-zA-Z0-9_-]+)'\\)");

	private final StringBuilder mText = new StringBuilder();
	private final Lexicon mLexicon = new Lexicon();

	public ACEText(String text) {
		tokenize(text);
	}

	public String getText() {
		return mText.toString();
	}

	public Lexicon getLexicon() {
		return mLexicon;
	}


	private void tokenize(String str) {
		for (String tok : str.split("\\s+")) {
			Matcher m = PN_SG.matcher(tok);
			if (m.matches()) {
				add(m.group(1), LexiconEntry.createPropernameSgEntry(m.group(1), m.group(2), Gender.create(m.group(3))));
				continue;
			}
			m = NOUN_SG.matcher(tok);
			if (m.matches()) {
				add(m.group(1), LexiconEntry.createNounSgEntry(m.group(1), m.group(2), Gender.create(m.group(3))));
				continue;
			}
			m = NOUN_PL.matcher(tok);
			if (m.matches()) {
				add(m.group(1), LexiconEntry.createNounPlEntry(m.group(1), m.group(2), Gender.create(m.group(3))));
				continue;
			}
			m = TV_FINSG.matcher(tok);
			if (m.matches()) {
				add(m.group(1), LexiconEntry.createTrVerbThirdEntry(m.group(1), m.group(2)));
				continue;
			}
			m = TV_INFPL.matcher(tok);
			if (m.matches()) {
				add(m.group(1), LexiconEntry.createTrVerbInfEntry(m.group(1), m.group(2)));
				continue;
			}
			m = TV_PP.matcher(tok);
			if (m.matches()) {
				add(m.group(1), LexiconEntry.createTrVerbPPEntry(m.group(1), m.group(2)));
				continue;
			}
			mText.append(' ');
			mText.append(tok);
		}
	}


	private void add(String tok, LexiconEntry le) {
		mLexicon.addEntry(le);
		mText.append(' ');
		mText.append("`");
		mText.append(tok);
		mText.append("`");
	}
}