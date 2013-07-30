package ch.uzh.ifi.attempto.ape;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;

import static org.junit.Assert.*;

public class APEWebserviceTest {
    @SuppressWarnings("UnusedDeclaration")
    private static Logger logger = LoggerFactory.getLogger(APEWebserviceTest.class);

    private static final String ACETEXT_LONG_NO_LEXICON_DRS = "drs([A],[object(A,n1,countable,na,eq,1)-1/45,property(A,a1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890,pos)-1/42])";
    private static final String NOUN = "n1";
    private static final String ADJ = "a1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890";
    private static final String APEWS_URL = "http://attempto.ifi.uzh.ch/ws/ape/apews.perl";
    // It is expected that "ape.exe httpserver" runs on localhost:8000
    private static final String APEWS_URL_LOCALHOST = "http://localhost:8000";
    private static final String WRONG_URL = "http://attempto.ifiuzh.ch/ws/apews.perl";
    private static final String ACETEXT_UTF8 = "John sees \"✈\".";
    private static final String ACETEXT_WRONG = "There is there is.";
    private static final String ACETEXT_UTF8_DRS = "drs([A],[predicate(A,see,named('John'),string('✈'))-1/2])";
    private static final String ACETEXT_LONG_DRS = "drs([A],[object(A," + NOUN + ",countable,na,eq,1)-1/23,property(A," + ADJ + ",pos)-1/22])";

    // Should be longer than MAX_HTTP_GET_LENGTH
    private static final String ACETEXT_LONG = "There is a " +
            ADJ + " and " +
            ADJ + " and " +
            ADJ + " and " +
            ADJ + " and " +
            ADJ + " and " +
            ADJ + " and " +
            ADJ + " and " +
            ADJ + " and " +
            ADJ + " and " +
            ADJ + " " + NOUN + ".";

    private static Process serverProcess;

    @BeforeClass
    public static void setUp() throws IOException {
        String executableName = "../ape.exe";
        serverProcess = Runtime.getRuntime().exec(new String[]{executableName, "-httpserver", "-port", "8000"});

        assertNotNull(serverProcess);

    }

    @AfterClass
    public static void tearDown() throws Exception {
        assertNotNull(serverProcess);
        serverProcess.getOutputStream().close();
        int status = serverProcess.waitFor();
        assertEquals(0, status);
    }


    @Test
    public final void testGetSoloOutput() throws ACEParserException {
        ACEParser ap = new APEWebservice(APEWS_URL);
        String result = null;

        result = ap.getSoloOutput(Testcase.ACETEXT, OutputType.DRS);

        assertEquals(Testcase.ACETEXT_DRS, result.trim());
    }

    @Test
    public final void testGetSoloOutputUtf8() {
        ACEParser ap = new APEWebservice(APEWS_URL);
        String result = null;
        try {
            result = ap.getSoloOutput(ACETEXT_UTF8, OutputType.DRS);
        } catch (ACEParserException e) {
        }
        assertEquals(ACETEXT_UTF8_DRS, result.trim());
    }

    @Test
    public final void testAPEWebservice1() {
        ACEParser ap = new APEWebservice(WRONG_URL);
        try {
            ap.getSoloOutput(Testcase.ACETEXT, OutputType.DRS);
            fail("Should throw an exception");
        } catch (ACEParserException e) {
            fail("Should NOT throw ACEParserException");
        } catch (RuntimeException e) {

        }
    }

    @Test
    public final void testAPEWebservice2() {
        ACEParser ap = new APEWebservice(APEWS_URL);
        try {
            ap.getSoloOutput(ACETEXT_WRONG, OutputType.DRS);
            fail("Should throw ACEParserException");
        } catch (ACEParserException e) {
        }
    }

    @Test
    public final void testGetSoloOutputLocalhost() throws ACEParserException {
        ACEParser ap = new APEWebservice(APEWS_URL_LOCALHOST);
        String result = ap.getSoloOutput(Testcase.ACETEXT, OutputType.DRS);
        assertEquals(Testcase.ACETEXT_DRS, result.trim());
    }


    @Test
    public final void testGetSoloOutputLong() throws ACEParserException {
        ACEParser ap = new APEWebservice(APEWS_URL);
        Lexicon lexicon = createLexicon();
        String result = null;
        result = ap.getSoloOutput(ACETEXT_LONG, lexicon, OutputType.DRS);

    }

    @Test
    public final void testGetSoloOutputLocalhostLongNoLexicon() throws ACEParserException {

        ACEParser ap = new APEWebservice(APEWS_URL_LOCALHOST);
        String result = null;
        ap.setGuessingEnabled(true);
        result = ap.getSoloOutput(ACETEXT_LONG, OutputType.DRS);
        assertEquals("long drs, no lexicon (guessing)", ACETEXT_LONG_NO_LEXICON_DRS, result);

    }

    @Test
    public final void testGetSoloOutputLocalhostLong() throws ACEParserException {

        ACEParser ap = new APEWebservice(APEWS_URL_LOCALHOST);
        Lexicon lexicon = createLexicon();
        String result = null;
        result = ap.getSoloOutput(ACETEXT_LONG, lexicon, OutputType.DRS);
        assertEquals("long drs", ACETEXT_LONG_DRS, result);

        assertEquals(ACETEXT_LONG_DRS, result.trim());
    }


    @Test
    public final void testGetMultiOutput() {
        runMultiOutputTestForURL(APEWS_URL);
    }

    @Test
    public final void testGetMultiOutputLocalhost() {
        runMultiOutputTestForURL(APEWS_URL_LOCALHOST);
    }

    private void runMultiOutputTestForURL(String url) {
        ACEParser ap = new APEWebservice(url);
        ACEParserResult result = ap.getMultiOutput(Testcase.ACETEXT, OutputType.DRS, OutputType.TPTP);
        assertEquals(Testcase.ACETEXT_DRS, result.get(OutputType.DRS));
        assertEquals(Testcase.ACETEXT_TPTP, result.get(OutputType.TPTP));
    }


    @Test
    public final void testGetMultiOutput3() {
        ACEParser ap = new APEWebservice(APEWS_URL);
        ACEParserResult response = ap.getMultiOutput(Testcase.ACETEXT, OutputType.DRSPP);
        assertEquals(Testcase.ACETEXT_DRSPP, response.get(OutputType.DRSPP).trim());
    }


    private Lexicon createLexicon() {
        Lexicon lexicon = new Lexicon();
        lexicon.addEntry(LexiconEntry.createNounSgEntry(NOUN, NOUN, Gender.NEUTRAL));
        lexicon.addEntry(LexiconEntry.createAdjEntry(ADJ, ADJ));
        return lexicon;
    }
}
