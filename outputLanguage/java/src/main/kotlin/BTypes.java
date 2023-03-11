import java.util.Collection;

public class BTypes {
    // TODO: move
    public static Number min(Collection<Number> collection) {
        Number min = Long.MAX_VALUE;
        for (Number entry: collection) {
            if (entry instanceof Integer && (Integer) entry < min.longValue()) {
                min = entry;
            } else if (entry instanceof Double && (Double) entry < min.longValue()) {
                min = entry;
            } else if (entry instanceof Long && (Long) entry < min.longValue()) {
                min = entry;
            } else if (entry instanceof Float && (Float) entry < min.longValue()) {
                min = entry;
            }
        }
        return min;
    }
}