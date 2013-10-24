package ch.uzh.ifi.attempto.ape;

import org.junit.Test;

import static org.junit.Assert.*;

public class LexiconEntryTest {

    @Test
    public final void testCreateAdvEntry() {
        LexiconEntry le = LexiconEntry.createAdvEntry(Testcase.WORD, Testcase.SYMBOL);
        assertEquals(le.toString(), Testcase.ENTRY);
    }

    @Test
    public final void testCreateAdvEntryEquals() {
        LexiconEntry le1 = LexiconEntry.createAdvEntry(Testcase.WORD, Testcase.SYMBOL);
        LexiconEntry le2 = LexiconEntry.createAdvEntry(Testcase.WORD, Testcase.SYMBOL);
        assertEquals(le1, le2);
    }

    @Test
    public final void testCreateAdvEntryHashCode() {
        LexiconEntry le1 = LexiconEntry.createAdvEntry(Testcase.WORD, Testcase.SYMBOL);
        LexiconEntry le2 = LexiconEntry.createAdvEntry(Testcase.WORD, Testcase.SYMBOL);
        assertEquals(le1.hashCode(), le2.hashCode());
    }

}