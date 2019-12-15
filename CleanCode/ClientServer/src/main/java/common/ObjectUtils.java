package common;

import java.io.Closeable;
import java.io.IOException;

public class ObjectUtils {

    public static void closeIgnoringException(Closeable closeable) {
        if (closeable != null)
            try {
                closeable.close();
            } catch (IOException ignore) {
            }
    }
}
