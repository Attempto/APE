package ch.uzh.ifi.attempto.ape;

import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

/**
 * Created with IntelliJ IDEA.
 * User: ses
 * Date: 2/2/13
 * Time: 2:54 AM
 * To change this template use File | Settings | File Templates.
 */

public class ApeLocalTest extends Testcase {

    @Test
    public void testCreateLocal() throws ACEParserException {
        APELocal apeLocal = getApeLocal();
        assertNotNull(apeLocal);
    }

    private APELocal getApeLocal() {
        APELocal.init("../ape.exe");
        return APELocal.getInstance();
    }

    @Test
    public void testDRS() throws ACEParserException {
        APELocal apeLocal = getApeLocal();
        apeLocal.setGuessingEnabled(true);
        String drs = apeLocal.getSoloOutput(ACETEXT, OutputType.DRS);
        assertEquals("DRS mismatch", ACETEXT_DRS, drs);
    }

}
